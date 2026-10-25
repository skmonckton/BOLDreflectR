// Alignment viewer for BOLDreflectR
// Draws the alignment on canvases, rendering only the visible part (so large alignments stay responsive), and
// supports basic AliView-style manual editing:
//   click/drag to select a block, click names to select whole sequences (Shift to extend), click the ruler to select columns
//   Space            insert a gap before the selection (in the selected sequences)
//   Backspace        remove a gap just before the selection
//   Delete           remove the selected positions, if they are all gaps
//   Ctrl+Left/Right  slide the selected residues into adjacent gaps (or Ctrl+drag the selection)
//   Arrow keys       move the selection; Ctrl+A select all; Ctrl+C copy selection; Ctrl+Z undo
//   Ctrl+scroll      zoom (also trackpad pinch)
// Copy/save/undo/remove-gap-column buttons are in the app's bottom button bar and call BRAlignmentViewer.call().
// Edits are sent to Shiny (input$aln_edited) so they can be used for tree building.
(function() {
  var COLOURS = {A: '#64F73F', C: '#FFB340', G: '#EB413C', T: '#3C88EE', U: '#3C88EE'};
  var OTHER = '#d0d0d0';
  var RULER = 20;
  var viewers = {};

  // copying goes through the app's clipboard handling on the server (input$viewer_copy), which uses the desktop
  // app's clipboard, the system clipboard, or the browser's, whichever is available
  function copyText(text, what) {
    Shiny.setInputValue('viewer_copy', {text: text, what: what}, {priority: 'event'});
  }

  // canvas sized for w x h CSS pixels; it's only resized (which clears and reallocates it) when the size changes,
  // not on every redraw while scrolling
  function fitCanvas(cv, w, h, dpr) {
    if (cv.width === Math.round(w * dpr) && cv.height === Math.round(h * dpr)) return;
    cv.width = Math.round(w * dpr); cv.height = Math.round(h * dpr);
    cv.style.width = w + 'px'; cv.style.height = h + 'px';
  }

  function AlignmentViewer(el, data) {
    this.el = el;
    this.names = data.names.slice();
    this.seqs = data.seqs.slice();
    this.marker = data.marker || 'alignment';
    this.version = data.version;
    this.font = 12;
    this.sel = null;
    this.anchor = null;
    this.history = [];
    this.edits = 0;
    this.build();
    this.normalize();
    this.layout();
    this.draw();
  }

  AlignmentViewer.prototype.build = function() {
    var self = this;
    this.el.innerHTML = '';
    this.el.classList.add('aln-viewer');
    this.el.tabIndex = 0;

    // narrow top band: editing buttons on the left; counts, help and zoom on the right
    // (copy and save buttons are in the app's bottom button bar; see ui.R)
    var top = document.createElement('div');
    top.className = 'viewer-topbar';
    // toolbar buttons return the focus to the viewer, so keyboard editing carries on
    var button = function(parent, icon, label, title, fn) {
      parent.appendChild(BRViewer.button('<i class="fas fa-' + icon + '"></i>' + (label ? ' ' + label : ''), title,
                                         function() { fn(); self.el.focus(); }));
    };
    var edits = document.createElement('span');
    edits.className = 'viewer-tools';
    button(edits, 'rotate-left', 'Undo', 'Undo the last edit (Ctrl+Z)', function() { self.undo(); });
    button(edits, 'eraser', 'Remove gap-only columns', 'Delete columns that contain only gaps', function() { self.removeGapColumns(); });
    top.appendChild(edits);
    var tools = document.createElement('span');
    tools.className = 'viewer-tools';
    this.status = document.createElement('span');
    this.status.className = 'viewer-status';
    tools.appendChild(this.status);
    tools.appendChild(BRViewer.helpIcon([
      'Click or drag to select; Shift+Click extends the selection',
      'Click names: select whole sequences',
      'Click the ruler: select columns',
      'Space: insert a gap before the selection',
      'Backspace: remove a gap just before the selection',
      'Delete: remove the selected positions, if they are all gaps',
      'Ctrl+←/→ or Ctrl+Click+Drag: slide the selected residues into adjacent gaps',
      'Arrow keys: move the selection',
      'Ctrl+A: select all',
      'Ctrl+C: copy the selection',
      'Ctrl+Z: undo',
      'Ctrl+Scroll or pinch: zoom'
    ]));
    button(tools, 'magnifying-glass-minus', '', 'Zoom out (Ctrl+scroll)', function() { self.zoom(-2); });
    button(tools, 'magnifying-glass-plus', '', 'Zoom in (Ctrl+scroll)', function() { self.zoom(2); });
    top.appendChild(tools);
    this.el.appendChild(top);

    var body = document.createElement('div');
    body.className = 'aln-body';
    this.namesWrap = document.createElement('div');
    this.namesWrap.className = 'aln-names';
    this.namesCanvas = document.createElement('canvas');
    this.namesWrap.appendChild(this.namesCanvas);
    this.seqWrap = document.createElement('div');
    this.seqWrap.className = 'aln-seqs';
    this.spacer = document.createElement('div');
    this.spacer.className = 'aln-spacer';
    this.seqCanvas = document.createElement('canvas');
    this.seqCanvas.className = 'aln-canvas';
    // canvas is sticky inside the full-size spacer, so it stays in view while the browser scrolls
    this.spacer.appendChild(this.seqCanvas);
    this.seqWrap.appendChild(this.spacer);
    body.appendChild(this.namesWrap);
    body.appendChild(this.seqWrap);
    this.el.appendChild(body);

    this.seqWrap.addEventListener('scroll', function() { self.draw(); });
    // Ctrl+Scroll (including trackpad pinch) zooms; otherwise names scroll with the sequences
    this.wheelAcc = 0;
    var onWheel = function(e, onNames) {
      if (e.ctrlKey) {
        e.preventDefault();
        self.wheelAcc += e.deltaY;
        if (Math.abs(self.wheelAcc) >= 40) {
          var rect = self.seqWrap.getBoundingClientRect();
          self.zoom(self.wheelAcc < 0 ? 2 : -2, onNames ? null : e.clientX - rect.left, e.clientY - rect.top);
          self.wheelAcc = 0;
        }
      } else if (onNames) {
        e.preventDefault();
        self.seqWrap.scrollTop += e.deltaY;
      }
    };
    this.seqWrap.addEventListener('wheel', function(e) { onWheel(e, false); }, {passive: false});
    this.namesWrap.addEventListener('wheel', function(e) { onWheel(e, true); }, {passive: false});
    this.seqCanvas.addEventListener('mousedown', function(e) { self.mouseDown(e, false); });
    this.namesCanvas.addEventListener('mousedown', function(e) { self.mouseDown(e, true); });
    this.namesCanvas.addEventListener('mousemove', function(e) {
      var r = self.rowAt(e.offsetY);
      self.namesCanvas.title = (r >= 0 && r < self.names.length) ? self.names[r] : '';
    });
    this.el.addEventListener('keydown', function(e) { self.keyDown(e); });
    if (window.ResizeObserver) {
      new ResizeObserver(function() { self.draw(); }).observe(this.seqWrap);
    }
  };

  // sizes depend on the zoom level
  AlignmentViewer.prototype.layout = function() {
    this.rh = Math.round(this.font * 1.4);
    this.cw = Math.max(2, Math.round(this.font * 0.8));
    this.spacer.style.width = (this.len * this.cw) + 'px';
    this.spacer.style.height = (RULER + this.seqs.length * this.rh) + 'px';
  };

  // zoom keeps position under the pointer in place, or else anchored to top-left corner
  AlignmentViewer.prototype.zoom = function(step, x, y) {
    x = (x === null || x === undefined) ? 0 : x;
    y = (y === null || y === undefined) ? RULER : Math.max(RULER, y);
    var w = this.seqWrap;
    var col = (w.scrollLeft + x) / this.cw, row = (w.scrollTop + y - RULER) / this.rh;
    var font = Math.min(22, Math.max(4, this.font + step));
    if (font === this.font) return;
    this.font = font;
    this.layout();
    w.scrollLeft = col * this.cw - x;
    w.scrollTop = row * this.rh - (y - RULER);
    this.draw();
  };

  // all sequences padded to the same length, trailing gap-only columns removed
  AlignmentViewer.prototype.normalize = function() {
    var len = 0, i;
    for (i = 0; i < this.seqs.length; i++) len = Math.max(len, this.seqs[i].length);
    for (i = 0; i < this.seqs.length; i++) {
      if (this.seqs[i].length < len) this.seqs[i] = this.seqs[i] + '-'.repeat(len - this.seqs[i].length);
    }
    var end = len;
    while (end > 0 && this.seqs.every(function(s) { return s.charAt(end - 1) === '-'; })) end--;
    if (end < len) this.seqs = this.seqs.map(function(s) { return s.slice(0, end); });
    this.len = end;
  };

  AlignmentViewer.prototype.rowAt = function(y) {
    return Math.floor((y - RULER + this.seqWrap.scrollTop) / this.rh);
  };

  AlignmentViewer.prototype.colAt = function(x) {
    return Math.floor((x + this.seqWrap.scrollLeft) / this.cw);
  };

  AlignmentViewer.prototype.draw = function() {
    var w = this.seqWrap.clientWidth, h = this.seqWrap.clientHeight;
    if (!w || !h) return;   // hidden (e.g. tab not shown yet)
    var dpr = window.devicePixelRatio || 1;
    var sl = this.seqWrap.scrollLeft, st = this.seqWrap.scrollTop;
    var cw = this.cw, rh = this.rh, n = this.seqs.length;
    var c0 = Math.floor(sl / cw), c1 = Math.min(this.len - 1, Math.ceil((sl + w) / cw));
    var r0 = Math.max(0, Math.floor(st / rh)), r1 = Math.min(n - 1, Math.ceil((st + h - RULER) / rh));

    // sequence area (canvas stays in view; content is drawn offset by scroll position)
    var cv = this.seqCanvas;
    fitCanvas(cv, w, h, dpr);
    var ctx = cv.getContext('2d');
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = '#fff';
    ctx.fillRect(0, 0, w, h);
    var letters = cw >= 7;
    ctx.font = this.font + 'px monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    for (var r = r0; r <= r1; r++) {
      var y = RULER + r * rh - st, s = this.seqs[r];
      for (var c = c0; c <= c1; c++) {
        var ch = s.charAt(c);
        if (ch === '-' || ch === '') continue;
        ctx.fillStyle = COLOURS[ch] || OTHER;
        ctx.fillRect(c * cw - sl, y, cw, rh);
        if (letters) {
          ctx.fillStyle = '#000';
          ctx.fillText(ch, c * cw - sl + cw / 2, y + rh / 2 + 1);
        }
      }
    }
    // selection
    if (this.sel) {
      var s0 = this.sel;
      ctx.fillStyle = 'rgba(0, 0, 0, 0.3)';
      ctx.fillRect(s0.c0 * cw - sl, RULER + s0.r0 * rh - st, (s0.c1 - s0.c0 + 1) * cw, (s0.r1 - s0.r0 + 1) * rh);
      ctx.strokeStyle = '#000';
      ctx.lineWidth = 1.5;
      ctx.strokeRect(s0.c0 * cw - sl, RULER + s0.r0 * rh - st, (s0.c1 - s0.c0 + 1) * cw, (s0.r1 - s0.r0 + 1) * rh);
    }
    // ruler (drawn last, so it stays on top)
    ctx.fillStyle = '#f2f2f2';
    ctx.fillRect(0, 0, w, RULER);
    ctx.fillStyle = '#555';
    ctx.strokeStyle = '#999';
    ctx.lineWidth = 1;
    ctx.font = '10px sans-serif';
    ctx.textAlign = 'center';
    var step = cw >= 6 ? 10 : (cw >= 3 ? 50 : 100);
    for (c = Math.max(0, c0 - (c0 % step)); c <= c1; c += step) {
      if (c === 0) continue;
      var x = (c - 1) * cw - sl + cw / 2;
      ctx.beginPath(); ctx.moveTo(x, RULER - 5); ctx.lineTo(x, RULER); ctx.stroke();
      ctx.fillText(String(c), x, 8);
    }

    // names
    var nc = this.namesCanvas, nw = this.namesWrap.clientWidth;
    fitCanvas(nc, nw, h, dpr);
    var nctx = nc.getContext('2d');
    nctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    nctx.fillStyle = '#fff';
    nctx.fillRect(0, 0, nw, h);
    nctx.font = Math.min(this.font, 13) + 'px sans-serif';
    nctx.textBaseline = 'middle';
    for (r = r0; r <= r1; r++) {
      y = RULER + r * rh - st;
      if (this.sel && r >= this.sel.r0 && r <= this.sel.r1) {
        nctx.fillStyle = '#dbe8f7';
        nctx.fillRect(0, y, nw, rh);
      }
      if (rh >= 7) {
        nctx.fillStyle = '#222';
        nctx.fillText(this.names[r], 4, y + rh / 2 + 1, nw - 8);
      }
    }
    nctx.fillStyle = '#f2f2f2';
    nctx.fillRect(0, 0, nw, RULER);
    nctx.fillStyle = '#555';
    nctx.font = '10px sans-serif';
    nctx.fillText(n + ' sequences', 4, RULER / 2);

    this.updateStatus();
  };

  AlignmentViewer.prototype.updateStatus = function() {
    var txt = this.seqs.length + ' sequences × ' + this.len + ' positions';
    if (this.sel) {
      var s = this.sel;
      if (s.r0 === s.r1) {
        var residue = this.seqs[s.r0].slice(0, s.c0 + 1).replace(/-/g, '').length;
        txt += '  |  ' + this.names[s.r0] + '  |  column ' + (s.c0 + 1) + (s.c1 > s.c0 ? '–' + (s.c1 + 1) : '') +
               (this.seqs[s.r0].charAt(s.c0) !== '-' ? ' (residue ' + residue + ')' : '');
      } else {
        txt += '  |  selected: ' + (s.r1 - s.r0 + 1) + ' sequences, columns ' + (s.c0 + 1) + '–' + (s.c1 + 1);
      }
    }
    this.status.textContent = txt;
  };

  // --- selection ---
  AlignmentViewer.prototype.setSel = function(r0, r1, c0, c1) {
    var n = this.seqs.length, L = this.len;
    this.sel = {
      r0: Math.max(0, Math.min(r0, r1)), r1: Math.min(n - 1, Math.max(r0, r1)),
      c0: Math.max(0, Math.min(c0, c1)), c1: Math.min(L - 1, Math.max(c0, c1))
    };
  };

  AlignmentViewer.prototype.mouseDown = function(e, onNames) {
    var self = this;
    e.preventDefault();
    this.el.focus();
    // the drag continues (and ends) outside the viewer too
    BRViewer.startDrag({move: function(ev) { self.mouseDrag(ev); }, end: function() { self.mouseUp(); }});
    var row = this.rowAt(e.offsetY), col = onNames ? 0 : this.colAt(e.offsetX);
    var ruler = e.offsetY < RULER;
    var s = this.sel;
    // Ctrl+drag slides the selected block as Ctrl+Left/Right, as one undoable edit
    // works when starting anywhere in the selected rows; elsewhere, the base under the pointer is selected and slid
    if ((e.ctrlKey || e.metaKey) && !onNames && !ruler && row >= 0 && row < this.seqs.length && col >= 0 && col < this.len) {
      if (!s || row < s.r0 || row > s.r1) {
        this.setSel(row, row, col, col);
        this.anchor = {row: row, col: col};
        s = this.sel;
        this.draw();
      }
      this.mode = 'slide';
      this.slideDrag = {startCol: col, moved: 0, before: {seqs: this.seqs.slice(), sel: Object.assign({}, s)}};
      this.dragging = true;
      this.seqCanvas.style.cursor = 'ew-resize';
      return;
    }
    this.mode = onNames ? 'rows' : (ruler ? 'cols' : 'cells');
    if (!(e.shiftKey && this.anchor)) this.anchor = {row: row, col: col};
    this.dragging = true;
    this.extendTo(row, col);
  };

  AlignmentViewer.prototype.mouseDrag = function(e) {
    var rect = (this.mode === 'rows' ? this.namesCanvas : this.seqCanvas).getBoundingClientRect();
    var x = e.clientX - rect.left, y = e.clientY - rect.top;
    // scroll while dragging past the edge
    if (this.mode !== 'slide') {
      if (y > rect.height - 10) this.seqWrap.scrollTop += this.rh;
      if (y < RULER && this.mode !== 'cols') this.seqWrap.scrollTop -= this.rh;
    }
    if (this.mode !== 'rows') {
      if (x > rect.width - 10) this.seqWrap.scrollLeft += this.cw * 2;
      if (x < 10) this.seqWrap.scrollLeft -= this.cw * 2;
    }
    if (this.mode === 'slide') {
      this.slideTo(this.colAt(x) - this.slideDrag.startCol);
      return;
    }
    this.extendTo(this.rowAt(y), this.mode === 'rows' ? 0 : this.colAt(x));
  };

  // move the block being Ctrl+dragged so it's offset by `target` columns from where the drag started
  // stops at the first position where it would run into residues
  AlignmentViewer.prototype.slideTo = function(target) {
    var d = this.slideDrag, changed = false;
    while (d.moved !== target) {
      var dir = target > d.moved ? 1 : -1;
      if (!this.slideStep(dir)) break;
      d.moved += dir;
      changed = true;
      this.normalize();
    }
    if (changed) {
      this.layout();
      this.draw();
    }
  };

  AlignmentViewer.prototype.mouseUp = function() {
    if (this.dragging && this.mode === 'slide') {
      this.seqCanvas.style.cursor = '';
      var d = this.slideDrag;
      if (d.moved !== 0) {
        this.history.push(d.before);
        if (this.history.length > 200) this.history.shift();
        this.changed();
      }
      this.slideDrag = null;
    }
    this.dragging = false;
  };

  AlignmentViewer.prototype.extendTo = function(row, col) {
    var a = this.anchor, n = this.seqs.length;
    if (this.mode === 'rows') this.setSel(a.row, row, 0, this.len - 1);
    else if (this.mode === 'cols') this.setSel(0, n - 1, a.col, col);
    else this.setSel(a.row, row, a.col, col);
    this.draw();
  };

  // --- editing ---
  AlignmentViewer.prototype.edit = function(fn) {
    var before = {seqs: this.seqs.slice(), sel: Object.assign({}, this.sel)};
    if (fn.call(this) === false) {
      this.flash();
      return;
    }
    this.history.push(before);
    if (this.history.length > 200) this.history.shift();
    this.normalize();
    this.layout();
    if (this.sel) this.setSel(this.sel.r0, this.sel.r1, this.sel.c0, this.sel.c1);
    this.draw();
    this.changed();
  };

  AlignmentViewer.prototype.undo = function() {
    var prev = this.history.pop();
    if (!prev) return;
    this.seqs = prev.seqs;
    this.sel = prev.sel && prev.sel.r0 !== undefined ? prev.sel : null;
    this.normalize();
    this.layout();
    this.draw();
    this.changed();
  };

  AlignmentViewer.prototype.flash = function() {
    var el = this.el;
    el.classList.add('aln-refused');
    setTimeout(function() { el.classList.remove('aln-refused'); }, 250);
  };

  // all selected rows have a gap at column c
  AlignmentViewer.prototype.gapsAt = function(c) {
    if (c < 0) return false;
    for (var r = this.sel.r0; r <= this.sel.r1; r++) {
      var ch = this.seqs[r].charAt(c);
      if (ch !== '-' && ch !== '') return false;
    }
    return true;
  };

  AlignmentViewer.prototype.insertGap = function() {
    this.edit(function() {
      var s = this.sel;
      for (var r = s.r0; r <= s.r1; r++) this.seqs[r] = this.seqs[r].slice(0, s.c0) + '-' + this.seqs[r].slice(s.c0);
      s.c0++; s.c1++;
    });
  };

  AlignmentViewer.prototype.removeGapBefore = function() {
    this.edit(function() {
      var s = this.sel;
      if (!this.gapsAt(s.c0 - 1)) return false;
      for (var r = s.r0; r <= s.r1; r++) this.seqs[r] = this.seqs[r].slice(0, s.c0 - 1) + this.seqs[r].slice(s.c0) + '-';
      s.c0--; s.c1--;
    });
  };

  AlignmentViewer.prototype.deleteSelectedGaps = function() {
    this.edit(function() {
      var s = this.sel;
      for (var c = s.c0; c <= s.c1; c++) if (!this.gapsAt(c)) return false;
      var w = s.c1 - s.c0 + 1;
      for (var r = s.r0; r <= s.r1; r++) this.seqs[r] = this.seqs[r].slice(0, s.c0) + this.seqs[r].slice(s.c1 + 1) + '-'.repeat(w);
      s.c1 = s.c0;
    });
  };

  // slide the selected block one position left (-1) or right (+1) into a gap (returns false if it can't move)
  // gaps at the edges of the selection are overwritten, as long as every row with residues in the selection has an adjacent gap
  // moving right past the last column adds a column
  AlignmentViewer.prototype.slideStep = function(dir) {
    var s = this.sel, spans = [], r, q, a, b;
    for (r = s.r0; r <= s.r1; r++) {
      q = this.seqs[r];
      for (a = s.c0; a <= s.c1 && (q.charAt(a) === '-' || q.charAt(a) === ''); a++);
      if (a > s.c1) continue;  // no residues in the selection
      for (b = s.c1; q.charAt(b) === '-' || q.charAt(b) === ''; b--);
      var next = dir < 0 ? a - 1 : b + 1;
      if (next < 0 || (next < q.length && q.charAt(next) !== '-')) return false;
      spans.push([r, a, b]);
    }
    if (spans.length === 0) return false;
    var len = 0;
    for (var i = 0; i < spans.length; i++) {
      r = spans[i][0]; a = spans[i][1]; b = spans[i][2];
      q = this.seqs[r];
      if (dir < 0) {
        this.seqs[r] = q.slice(0, a - 1) + q.slice(a, b + 1) + '-' + q.slice(b + 1);
      } else {
        if (q.length <= b + 1) q = q + '-';
        this.seqs[r] = q.slice(0, a) + '-' + q.slice(a, b + 1) + q.slice(b + 2);
      }
    }
    for (r = 0; r < this.seqs.length; r++) len = Math.max(len, this.seqs[r].length);
    // the selection moves with the block, staying within the alignment
    s.c0 = Math.max(0, s.c0 + dir);
    s.c1 = Math.min(len - 1, s.c1 + dir);
    return true;
  };

  AlignmentViewer.prototype.slide = function(dir) {
    this.edit(function() { return this.slideStep(dir); });
  };

  AlignmentViewer.prototype.removeGapColumns = function() {
    this.edit(function() {
      var keep = [], c, seqs = this.seqs;
      for (c = 0; c < this.len; c++) {
        if (!seqs.every(function(q) { return q.charAt(c) === '-'; })) keep.push(c);
      }
      if (keep.length === this.len) return false;
      this.seqs = seqs.map(function(q) { return keep.map(function(i) { return q.charAt(i); }).join(''); });
      this.sel = null;
    });
  };

  AlignmentViewer.prototype.keyDown = function(e) {
    var ctrl = e.ctrlKey || e.metaKey, s = this.sel, n = this.seqs.length;
    if (ctrl && e.key.toLowerCase() === 'z') { e.preventDefault(); this.undo(); return; }
    if (ctrl && e.key.toLowerCase() === 'a') { e.preventDefault(); this.setSel(0, n - 1, 0, this.len - 1); this.draw(); return; }
    if (!s) return;
    if (ctrl && e.key.toLowerCase() === 'c') { e.preventDefault(); this.copySelection(); return; }
    switch (e.key) {
      case ' ': e.preventDefault(); this.insertGap(); break;
      case 'Backspace': e.preventDefault(); this.removeGapBefore(); break;
      case 'Delete': e.preventDefault(); this.deleteSelectedGaps(); break;
      case 'ArrowLeft':
      case 'ArrowRight':
        e.preventDefault();
        var d = e.key === 'ArrowLeft' ? -1 : 1;
        if (ctrl) { this.slide(d); }
        else { this.setSel(s.r0, s.r1, Math.max(0, s.c0 + d), Math.min(this.len - 1, s.c1 + d)); this.scrollToSel(); this.draw(); }
        break;
      case 'ArrowUp':
      case 'ArrowDown':
        e.preventDefault();
        var dr = e.key === 'ArrowUp' ? -1 : 1;
        if (s.r0 + dr >= 0 && s.r1 + dr < n) { this.setSel(s.r0 + dr, s.r1 + dr, s.c0, s.c1); this.scrollToSel(); this.draw(); }
        break;
    }
  };

  AlignmentViewer.prototype.scrollToSel = function() {
    var s = this.sel, w = this.seqWrap;
    if (s.c0 * this.cw < w.scrollLeft) w.scrollLeft = s.c0 * this.cw;
    if ((s.c1 + 1) * this.cw > w.scrollLeft + w.clientWidth) w.scrollLeft = (s.c1 + 1) * this.cw - w.clientWidth;
    if (s.r0 * this.rh < w.scrollTop) w.scrollTop = s.r0 * this.rh;
    if (RULER + (s.r1 + 1) * this.rh > w.scrollTop + w.clientHeight) w.scrollTop = RULER + (s.r1 + 1) * this.rh - w.clientHeight;
  };

  // --- output ---
  AlignmentViewer.prototype.fasta = function(r0, r1, c0, c1) {
    var out = [];
    for (var r = r0; r <= r1; r++) out.push('>' + this.names[r] + '\n' + this.seqs[r].slice(c0, c1 + 1));
    return out.join('\n') + '\n';
  };

  AlignmentViewer.prototype.copySelection = function() {
    var s = this.sel;
    if (!s) return;
    copyText(this.fasta(s.r0, s.r1, s.c0, s.c1), (s.r1 - s.r0 + 1) + ' sequence(s)');
  };

  AlignmentViewer.prototype.copyAll = function() {
    copyText(this.fasta(0, this.seqs.length - 1, 0, this.len - 1), 'alignment');
  };

  AlignmentViewer.prototype.saveFasta = function() {
    var d = new Date(), pad = function(x) { return (x < 10 ? '0' : '') + x; };
    var name = 'alignment_' + this.marker + '_' + d.getFullYear() + pad(d.getMonth() + 1) + pad(d.getDate()) + '.fasta';
    var blob = new Blob([this.fasta(0, this.seqs.length - 1, 0, this.len - 1)], {type: 'text/plain'});
    var a = document.createElement('a');
    a.href = URL.createObjectURL(blob);
    a.download = name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function() { URL.revokeObjectURL(a.href); a.remove(); }, 1000);
  };

  AlignmentViewer.prototype.notify = function(msg) {
    Shiny.setInputValue('aln_notify', msg, {priority: 'event'});
  };

  // send the edited alignment to Shiny (max once per second)
  AlignmentViewer.prototype.changed = function() {
    var self = this;
    this.edits++;
    clearTimeout(this.syncTimer);
    this.syncTimer = setTimeout(function() {
      Shiny.setInputValue('aln_edited', {names: self.names, seqs: self.seqs, edits: self.edits, version: self.version});
    }, 1000);
  };

  // load a new alignment into the element with the given id (once the element exists; see BRViewer.loader)
  var loader = BRViewer.loader(function(el, msg) { viewers[msg.id] = new AlignmentViewer(el, msg); });
  Shiny.addCustomMessageHandler('aln_load', loader.load);
  window.BRAlignmentViewer = {
    viewers: viewers,
    // used by the buttons in the app's bottom button bar (ui.R)
    call: function(method) {
      var v = viewers.aln_viewer;
      if (!v) return;
      if (method === 'copySelection' && !v.sel) { v.notify('Select sequences first (click their names).'); return; }
      v[method]();
      v.el.focus();
    }
  };
})();

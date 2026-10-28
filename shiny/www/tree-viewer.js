// Tree viewer for BOLDreflectR
// Draws a rectangular phylogram as SVG in a scrollable area (one row per tip), with:
//   - tooltips for tips (record details) and branches (branch length, clade size)
//   - row zoom (buttons, Ctrl+scroll or pinch) and width zoom (buttons, Ctrl+Shift+scroll)
//   - a search box that highlights tips whose labels match (Enter for the next match)
//   - a scale bar pinned to the top-right corner, and an overview inset of the whole tree (below it) showing
//     the visible area; clicking or dragging in the inset moves the view
// The data (TSV/CSV/XLSX) and tree (PDF/Newick) download buttons are in the app's bottom button bar. Layout
// coordinates come from tree_layout() in R/tree.R; tip labels and colours can be updated without reloading.
(function() {
  var NS = 'http://www.w3.org/2000/svg';
  var INSET_W = 150, INSET_H = 200;
  var esc = BRViewer.esc;

  // a round scale bar length, roughly a fifth of the tree depth
  function niceScale(maxX) {
    if (!(maxX > 0)) return 0;
    var raw = maxX / 5, p = Math.pow(10, Math.floor(Math.log10(raw))), m = raw / p;
    return (m >= 5 ? 5 : m >= 2 ? 2 : 1) * p;
  }

  function TreeViewer(el, t) {
    this.el = el;
    this.t = t;
    this.n = t.parent.length;
    this.initialRowH = this.rowH = t.n_tip > 400 ? 14 : 18;
    this.widthFactor = 1;
    this.matches = [];
    this.matchIdx = -1;
    this.wheelAcc = 0;
    // vertical extent of each internal node's connector, from its first to its last child (fixed for a tree)
    this.ymin = new Array(this.n).fill(Infinity);
    this.ymax = new Array(this.n).fill(-Infinity);
    for (var i = 0; i < this.n; i++) {
      var p = t.parent[i];
      if (p < 0) continue;
      this.ymin[p] = Math.min(this.ymin[p], t.y[i]);
      this.ymax[p] = Math.max(this.ymax[p], t.y[i]);
    }
    this.measureLabels();
    this.build();
    this.render();
  }

  // longest label sets the space reserved for labels
  TreeViewer.prototype.measureLabels = function() {
    var longest = 0, t = this.t;
    for (var i = 0; i < this.n; i++) if (t.tip[i]) longest = Math.max(longest, t.label[i].length);
    this.labelChars = longest;
  };

  // new tip labels and tooltip details (tips only, in node order) and node colours ('' for none), keeping zoom and scroll position
  TreeViewer.prototype.update = function(label, info, col) {
    var t = this.t;
    for (var i = 0; i < label.length; i++) { t.label[i] = label[i]; t.info[i] = info[i]; }
    for (i = 0; i < this.n; i++) t.col[i] = (col && col[i]) || '';
    this.measureLabels();
    var top = this.scroll.scrollTop, left = this.scroll.scrollLeft;
    this.render();
    this.scroll.scrollTop = top;
    this.scroll.scrollLeft = left;
  };

  TreeViewer.prototype.build = function() {
    var self = this;
    this.el.innerHTML = '';
    this.el.classList.add('tree-viewer');

    // narrow top band: info on the left; zoom and search on the right
    var top = document.createElement('div');
    top.className = 'viewer-topbar';
    this.status = document.createElement('span');
    this.status.className = 'viewer-status';
    top.appendChild(this.status);
    var tools = document.createElement('span');
    tools.className = 'viewer-tools';
    tools.appendChild(BRViewer.helpIcon([
      'Hover over tips and branches for details',
      'Ctrl+Scroll or pinch: adjust row height',
      'Ctrl+Shift+Scroll: adjust tree width',
      'Find: highlight tips with matching text in labels; Enter jumps to the next one',
      'Overview panel: click or drag to move around the tree'
    ]));
    var iconButton = function(html, title, fn) { tools.appendChild(BRViewer.button(html, title, fn)); };
    // icons for narrower, wider, and reset
    iconButton('<svg class="viewer-svg-icon" viewBox="0 0 16 16" aria-hidden="true"><path d="M1 3.5 6.5 8 1 12.5Z M15 3.5 9.5 8 15 12.5Z"/></svg>',
               'Narrower (Ctrl+Shift+scroll)', function() { self.zoomWidth(1 / 1.25); });
    iconButton('<svg class="viewer-svg-icon" viewBox="0 0 16 16" aria-hidden="true"><path d="M6.5 3.5 1 8 6.5 12.5Z M9.5 3.5 15 8 9.5 12.5Z"/></svg>',
               'Wider (Ctrl+Shift+scroll)', function() { self.zoomWidth(1.25); });
    iconButton('<i class="fas fa-rotate-left"></i>', 'Reset width and row height', function() { self.resetZoom(); });
    iconButton('<i class="fas fa-magnifying-glass-minus"></i>', 'Smaller rows (Ctrl+scroll)', function() { self.zoomRows(-2); });
    iconButton('<i class="fas fa-magnifying-glass-plus"></i>', 'Larger rows (Ctrl+scroll)', function() { self.zoomRows(2); });
    this.find = document.createElement('input');
    this.find.type = 'search';
    this.find.placeholder = 'Find tip (Enter for next)';
    this.find.className = 'form-control form-control-sm tree-find';
    this.find.addEventListener('input', function() { self.search(); });
    this.find.addEventListener('keydown', function(e) { if (e.key === 'Enter') { e.preventDefault(); self.nextMatch(); } });
    tools.appendChild(this.find);
    top.appendChild(tools);
    this.el.appendChild(top);

    var area = document.createElement('div');
    area.className = 'tree-area';
    this.scroll = document.createElement('div');
    this.scroll.className = 'tree-scroll';
    area.appendChild(this.scroll);
    // scale bar and overview sit above/in front of the scrolling area, so they stay in place
    this.scalebar = document.createElement('div');
    this.scalebar.className = 'tree-scalebar';
    area.appendChild(this.scalebar);
    this.inset = document.createElement('canvas');
    this.inset.className = 'tree-inset';
    this.inset.title = 'Overview: click or drag to move around the tree';
    area.appendChild(this.inset);
    this.el.appendChild(area);

    this.tip = document.createElement('div');
    this.tip.className = 'tree-tooltip';
    this.el.appendChild(this.tip);

    this.scroll.addEventListener('mouseover', function(e) { self.hover(e); });
    this.scroll.addEventListener('mousemove', function(e) { self.moveTip(e); });
    this.scroll.addEventListener('mouseout', function(e) { self.unhover(e); });
    this.scroll.addEventListener('scroll', function() { self.drawInsetView(); });
    this.scroll.addEventListener('wheel', function(e) {
      if (!e.ctrlKey) return;
      e.preventDefault();
      self.wheelAcc += e.deltaY;
      if (Math.abs(self.wheelAcc) < 40) return;
      var rect = self.scroll.getBoundingClientRect();
      if (e.shiftKey) self.zoomWidth(self.wheelAcc < 0 ? 1.25 : 1 / 1.25, e.clientX - rect.left);
      else self.zoomRows(self.wheelAcc < 0 ? 2 : -2, e.clientY - rect.top);
      self.wheelAcc = 0;
    }, {passive: false});
    var insetMove = function(e) {
      var r = self.inset.getBoundingClientRect();
      self.centreOn((e.clientX - r.left) / r.width, (e.clientY - r.top) / r.height);
    };
    this.inset.addEventListener('mousedown', function(e) {
      e.preventDefault();
      insetMove(e);
      BRViewer.startDrag({move: insetMove});
    });
    if (window.ResizeObserver) {
      var lastW = 0, lastH = 0;
      new ResizeObserver(function() {
        var w = self.scroll.clientWidth, h = self.scroll.clientHeight;
        if (w && (w !== lastW || h !== lastH)) { lastW = w; lastH = h; self.render(); }
      }).observe(this.scroll);
    }
  };

  // row height zoom according to pointer position
  TreeViewer.prototype.zoomRows = function(step, y) {
    var rowH = Math.min(40, Math.max(2, this.rowH + step));
    if (rowH === this.rowH) return;
    if (y === undefined) y = this.scroll.clientHeight / 2;
    var row = (this.scroll.scrollTop + y - this.top) / this.rowH;
    this.rowH = rowH;
    this.render();
    this.scroll.scrollTop = row * this.rowH + this.top - y;
  };

  // width zoom according to pointer position
  TreeViewer.prototype.zoomWidth = function(factor, x) {
    var wf = Math.min(20, Math.max(0.25, this.widthFactor * factor));
    if (wf === this.widthFactor) return;
    if (x === undefined) x = this.scroll.clientWidth / 2;
    var pos = (this.scroll.scrollLeft + x - this.left) / this.sx;
    this.widthFactor = wf;
    this.render();
    this.scroll.scrollLeft = pos * this.sx + this.left - x;
  };

  // reset to initial row height and width, keeping visible part of tree in place
  TreeViewer.prototype.resetZoom = function() {
    var s = this.scroll, w = s.clientWidth / 2, h = s.clientHeight / 2;
    var row = (s.scrollTop + h - this.top) / this.rowH, pos = (s.scrollLeft + w - this.left) / this.sx;
    this.rowH = this.initialRowH;
    this.widthFactor = 1;
    this.render();
    s.scrollTop = row * this.rowH + this.top - h;
    s.scrollLeft = pos * this.sx + this.left - w;
  };

  TreeViewer.prototype.render = function() {
    var t = this.t, n = this.n, rowH = this.rowH;
    var avail = this.scroll.clientWidth || 800;
    var font = Math.max(6, Math.min(13, rowH - 4));
    var labelW = rowH >= 7 ? Math.min(450, this.labelChars * font * 0.6 + 12) : 0;
    var left = 12, top = 12;
    var treeW = Math.max(100, (avail - left - labelW - 24) * this.widthFactor);
    var sx = t.max_x > 0 ? treeW / t.max_x : 1;
    var X = function(x) { return left + x * sx; };
    var Y = function(y) { return top + (y - 0.5) * rowH; };
    this.X = X; this.Y = Y; this.sx = sx; this.left = left; this.top = top;

    // branch paths are grouped by colour (t.col: colour of each node's branch and vertical line, '' = default)
    var paths = {'': []}, hits = [], labels = [], ymin = this.ymin, ymax = this.ymax;
    var add = function(i, d) { var c = t.col[i] || ''; (paths[c] = paths[c] || []).push(d); };
    for (var i = 0; i < n; i++) {
      var p = t.parent[i];
      if (p < 0) continue;
      // horizontal branch, plus a wider invisible line to make hovering easier
      add(i, 'M' + X(t.x[p]).toFixed(1) + ',' + Y(t.y[i]).toFixed(1) + 'H' + X(t.x[i]).toFixed(1));
      hits.push('<line class="tv-hit" data-i="' + i + '" x1="' + X(t.x[p]).toFixed(1) + '" y1="' + Y(t.y[i]).toFixed(1) +
                '" x2="' + Math.max(X(t.x[i]), X(t.x[p]) + 3).toFixed(1) + '" y2="' + Y(t.y[i]).toFixed(1) + '"/>');
    }
    for (i = 0; i < n; i++) {
      // vertical connector at each internal node
      if (ymax[i] > -Infinity) add(i, 'M' + X(t.x[i]).toFixed(1) + ',' + Y(ymin[i]).toFixed(1) + 'V' + Y(ymax[i]).toFixed(1));
      if (t.tip[i] && labelW > 0) {
        labels.push('<text class="tv-label" data-i="' + i + '" x="' + (X(t.x[i]) + 4).toFixed(1) + '" y="' + Y(t.y[i]).toFixed(1) +
                    '" font-size="' + font + '"' + (t.col[i] ? ' fill="' + esc(t.col[i]) + '"' : '') + '>' + esc(t.label[i]) + '</text>');
      }
    }
    var branchPaths = Object.keys(paths).map(function(c) {
      return '<path class="tv-branches"' + (c ? ' style="stroke:' + esc(c) + '"' : '') + ' d="' + paths[c].join('') + '"/>';
    }).join('');
    var width = left + treeW + labelW + 24, height = top + t.n_tip * rowH + 12;
    this.scroll.innerHTML = '<svg xmlns="' + NS + '" class="tv-svg" width="' + Math.ceil(width) + '" height="' + Math.ceil(height) + '">' +
      branchPaths + '<path class="tv-highlight" d=""/>' +
      '<g class="tv-hits">' + hits.join('') + '</g><g class="tv-marks"></g><g class="tv-labels">' + labels.join('') + '</g></svg>';
    this.highlight = this.scroll.querySelector('.tv-highlight');

    // scale bar (fixed in top-right corner)
    var sl = niceScale(t.max_x);
    this.scalebar.style.display = sl > 0 ? '' : 'none';
    if (sl > 0) {
      var len = Math.max(1, sl * sx);
      this.scalebar.innerHTML = '<svg width="' + Math.ceil(len + 2) + '" height="10"><path d="M1,1V9M1,5H' + (len + 1).toFixed(1) +
        'M' + (len + 1).toFixed(1) + ',1V9"/></svg><span>' + sl + '</span>';
    }
    this.drawInset();
    this.search(true);
  };

  // --- overview inset ---
  TreeViewer.prototype.drawInset = function() {
    var t = this.t, n = this.n, svg = this.scroll.querySelector('.tv-svg');
    var needed = svg && (svg.height.baseVal.value > this.scroll.clientHeight * 1.1 || svg.width.baseVal.value > this.scroll.clientWidth * 1.1);
    this.inset.style.display = needed ? '' : 'none';
    if (!needed) return;
    var dpr = window.devicePixelRatio || 1;
    var w = INSET_W, h = INSET_H;
    // tree drawing is cached so only the view/highlight box is redrawn when scrolling
    var cache = document.createElement('canvas');
    cache.width = w * dpr; cache.height = h * dpr;
    var ctx = cache.getContext('2d');
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.fillStyle = '#fff';
    ctx.fillRect(0, 0, w, h);
    // same proportions as full drawing
    var sw = svg.width.baseVal.value, sh = svg.height.baseVal.value, X = this.X, Y = this.Y;
    var fx = function(x) { return X(x) / sw * w; }, fy = function(y) { return Y(y) / sh * h; };
    ctx.lineWidth = 0.6;
    // segments grouped by colour as in main drawing
    var segs = {}, ymin = this.ymin, ymax = this.ymax;
    var seg = function(i, x0, y0, x1, y1) { var c = t.col[i] || '#555'; (segs[c] = segs[c] || []).push([x0, y0, x1, y1]); };
    for (var i = 0; i < n; i++) {
      var p = t.parent[i];
      if (p >= 0) seg(i, fx(t.x[p]), fy(t.y[i]), fx(t.x[i]), fy(t.y[i]));
    }
    for (i = 0; i < n; i++) {
      if (ymax[i] > -Infinity) seg(i, fx(t.x[i]), fy(ymin[i]), fx(t.x[i]), fy(ymax[i]));
    }
    Object.keys(segs).forEach(function(c) {
      ctx.strokeStyle = c;
      ctx.beginPath();
      segs[c].forEach(function(s) { ctx.moveTo(s[0], s[1]); ctx.lineTo(s[2], s[3]); });
      ctx.stroke();
    });
    this.insetCache = cache;
    this.inset.width = w * dpr; this.inset.height = h * dpr;
    this.inset.style.width = w + 'px'; this.inset.style.height = h + 'px';
    this.drawInsetView();
  };

  TreeViewer.prototype.drawInsetView = function() {
    if (!this.insetCache || this.inset.style.display === 'none') return;
    var svg = this.scroll.querySelector('.tv-svg');
    var dpr = window.devicePixelRatio || 1, w = INSET_W, h = INSET_H, s = this.scroll;
    var ctx = this.inset.getContext('2d');
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.drawImage(this.insetCache, 0, 0);
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    var sw = svg.width.baseVal.value, sh = svg.height.baseVal.value;
    var x = s.scrollLeft / sw * w, y = s.scrollTop / sh * h;
    var vw = Math.min(1, s.clientWidth / sw) * w, vh = Math.min(1, s.clientHeight / sh) * h;
    ctx.fillStyle = 'rgba(230, 81, 0, 0.12)';
    ctx.fillRect(x, y, vw, vh);
    ctx.strokeStyle = '#e65100';
    ctx.lineWidth = 1.5;
    ctx.strokeRect(x + 0.75, y + 0.75, Math.max(2, vw - 1.5), Math.max(2, vh - 1.5));
  };

  // centre the view on a point of the inset (fractions of its width and height)
  TreeViewer.prototype.centreOn = function(fx, fy) {
    var svg = this.scroll.querySelector('.tv-svg'), s = this.scroll;
    s.scrollLeft = fx * svg.width.baseVal.value - s.clientWidth / 2;
    s.scrollTop = fy * svg.height.baseVal.value - s.clientHeight / 2;
  };

  // --- tooltips ---
  TreeViewer.prototype.hover = function(e) {
    var target = e.target, i = target.getAttribute && target.getAttribute('data-i');
    if (i === null || i === undefined) return;
    i = +i;
    var t = this.t, html;
    if (target.classList.contains('tv-label') || t.tip[i]) {
      html = '<b>Tip ' + t.y[i] + '</b>' + (t.info[i] ? '<br>' + t.info[i] : '') +
             '<br>Branch length: ' + t.bl[i].toFixed(5);
    } else {
      html = 'Branch length: ' + t.bl[i].toFixed(5) + '<br>Tips in clade: ' + t.ntips[i];
    }
    this.tip.innerHTML = html;
    this.tip.style.display = 'block';
    // highlight the branch leading to hovered node
    var p = t.parent[i];
    if (p >= 0) this.highlight.setAttribute('d', 'M' + this.X(t.x[p]) + ',' + this.Y(t.y[i]) + 'H' + this.X(t.x[i]));
    this.moveTip(e);
  };

  TreeViewer.prototype.moveTip = function(e) {
    if (this.tip.style.display !== 'block') return;
    var x = e.clientX + 14, y = e.clientY + 14, w = this.tip.offsetWidth, h = this.tip.offsetHeight;
    if (x + w > window.innerWidth - 4) x = e.clientX - w - 14;
    if (y + h > window.innerHeight - 4) y = e.clientY - h - 14;
    this.tip.style.left = x + 'px';
    this.tip.style.top = y + 'px';
  };

  TreeViewer.prototype.unhover = function(e) {
    if (e.target.getAttribute && e.target.getAttribute('data-i') !== null) {
      this.tip.style.display = 'none';
      this.highlight.setAttribute('d', '');
    }
  };

  // --- search ---
  TreeViewer.prototype.search = function(keepPosition) {
    var q = this.find.value.trim().toLowerCase(), t = this.t, self = this;
    // matching labels get a yellow highlight (drawn as rectangles)
    var marks = this.scroll.querySelector('.tv-marks'), rects = [];
    this.matches = [];
    if (q) {
      for (var i = 0; i < this.n; i++) {
        if (t.tip[i] && t.label[i].toLowerCase().indexOf(q) >= 0) this.matches.push(i);
      }
      this.matches.sort(function(a, b) { return t.y[a] - t.y[b]; });
      var labels = {};
      this.scroll.querySelectorAll('.tv-label').forEach(function(el) { labels[el.getAttribute('data-i')] = el; });
      this.matches.forEach(function(i) {
        var el = labels[i];
        if (!el) return;
        var b = el.getBBox();
        rects.push('<rect x="' + (b.x - 2).toFixed(1) + '" y="' + b.y.toFixed(1) + '" width="' + (b.width + 4).toFixed(1) +
                   '" height="' + b.height.toFixed(1) + '"/>');
      });
      this.status.textContent = this.matches.length + ' of ' + t.n_tip + ' tips match';
    } else {
      this.status.textContent = t.n_tip + ' tips';
    }
    if (marks) marks.innerHTML = rects.join('');
    if (q && !keepPosition) { this.matchIdx = -1; this.nextMatch(); }
  };

  TreeViewer.prototype.nextMatch = function() {
    if (!this.matches.length) return;
    this.matchIdx = (this.matchIdx + 1) % this.matches.length;
    var i = this.matches[this.matchIdx];
    this.scroll.scrollTop = Math.max(0, this.Y(this.t.y[i]) - this.scroll.clientHeight / 2);
    this.status.textContent = (this.matchIdx + 1) + ' of ' + this.matches.length + ' matching tips';
  };

  // load a tree into the element with the given id (once the element exists; see BRViewer.loader), replacing any
  // earlier one
  var loader = BRViewer.loader(function(el, msg) { el._treeViewer = new TreeViewer(el, msg.tree); });
  Shiny.addCustomMessageHandler('tree_load', loader.load);
  // new tip labels, tooltips or colours for the tree shown (if still loading, the pending tree is updated instead)
  Shiny.addCustomMessageHandler('tree_update', function(msg) {
    var label = [].concat(msg.label), info = [].concat(msg.info), col = Array.isArray(msg.col) ? msg.col : null;
    if (loader.pending[msg.id]) {
      var t = loader.pending[msg.id].tree;
      for (var i = 0; i < label.length; i++) { t.label[i] = label[i]; t.info[i] = info[i]; }
      t.col = t.col.map(function(c, i) { return (col && col[i]) || ''; });
      return;
    }
    var el = document.getElementById(msg.id);
    if (el && el._treeViewer) el._treeViewer.update(label, info, col);
  });
})();

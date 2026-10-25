// Shared by the alignment and tree viewers (www/alignment-viewer.js, www/tree-viewer.js)
window.BRViewer = (function() {
  function esc(x) {
    return String(x).replace(/[&<>"']/g, function(c) {
      return {'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[c];
    });
  }

  // small toolbar button (html: its content, e.g. an icon)
  function button(html, title, fn) {
    var b = document.createElement('button');
    b.type = 'button';
    b.className = 'btn btn-default btn-sm viewer-icon-btn';
    b.innerHTML = html;
    b.title = title;
    b.addEventListener('click', fn);
    return b;
  }

  // help icon with a tooltip shown on hover
  // first line is a heading in bold (apart from any text in parentheses), following lines have bold labels (i.e. text preceding a colon)
  function helpIcon(lines) {
    var html = lines.map(function(line, i) {
      var m = i === 0 ? line.match(/^([^(]*)(\(.*)?$/) : line.match(/^([^:]*:)(.*)$/);
      return '<div' + (i === 0 ? ' class="viewer-help-title"' : '') + '>' +
        (m ? '<b>' + esc(m[1]) + '</b>' + esc(m[2] || '') : esc(line)) + '</div>';
    }).join('');
    var help = document.createElement('span');
    help.className = 'viewer-help';
    help.innerHTML = '<i class="fas fa-circle-question"></i><div class="viewer-help-tip">' + html + '</div>';
    return help;
  }

  // dragging that continues outside the viewer
  // one set of window listeners for all viewers, passed on to the handlers of the drag in progress ({move: fn(e), end: fn(e)})
  var drag = null;
  window.addEventListener('mousemove', function(e) { if (drag && drag.move) drag.move(e); });
  window.addEventListener('mouseup', function(e) {
    var d = drag;
    drag = null;
    if (d && d.end) d.end(e);
  });
  function startDrag(handlers) { drag = handlers; }

  // messages that load a viewer into an element by id
  // the element may not exist yet (e.g. while its tab is being inserted), so this waits without a time limit
  // create(el, msg) makes the viewer; pending holds loads still waiting, by element id
  function loader(create) {
    var pending = {}, observer = null;
    function tryPending() {
      Object.keys(pending).forEach(function(id) {
        var el = document.getElementById(id);
        if (!el) return;
        var msg = pending[id];
        delete pending[id];
        create(el, msg);
      });
      if (Object.keys(pending).length === 0 && observer) { observer.disconnect(); observer = null; }
    }
    return {
      pending: pending,
      load: function(msg) {
        pending[msg.id] = msg;
        tryPending();
        if (Object.keys(pending).length > 0 && !observer) {
          observer = new MutationObserver(tryPending);
          observer.observe(document.body, {childList: true, subtree: true});
        }
      }
    };
  }

  return {esc: esc, button: button, helpIcon: helpIcon, startDrag: startDrag, loader: loader};
})();

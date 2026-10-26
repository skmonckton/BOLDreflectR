// Taxonomic summary tree
// rows are built by tax_tree() in global.R, parents first, with depth in data-depth
// search shows matching taxa with all their parents and descendants (copying/saving then uses filtered rows)
(function() {
  function rows(tree) {
    return Array.prototype.slice.call(tree.querySelectorAll('.tax-row'));
  }

  function update(tree) {
    var rs = rows(tree);
    var q = tree.querySelector('.tax-search').value.trim().toLowerCase();
    var depth = rs.map(function(r) { return +r.dataset.depth; });
    var show = rs.map(function() { return !q; });
    rs.forEach(function(r) { r.taxOnPath = false; });
    if (q) {
      // ancestors: the rows on the stack; descendants: rows deeper than the nearest matching ancestor
      var stack = [], matchDepth = Infinity;
      rs.forEach(function(r, i) {
        while (stack.length && depth[stack[stack.length - 1]] >= depth[i]) stack.pop();
        if (depth[i] <= matchDepth) matchDepth = Infinity;
        if (depth[i] > matchDepth) show[i] = true;
        var match = r.dataset.name.indexOf(q) >= 0;
        r.classList.toggle('tax-match', match);
        if (match) {
          show[i] = true;
          stack.forEach(function(a) { show[a] = true; rs[a].taxOnPath = true; });
          matchDepth = Math.min(matchDepth, depth[i]);
        }
        stack.push(i);
      });
    } else {
      rs.forEach(function(r) { r.classList.remove('tax-match'); });
    }
    // collapsed branches hide their descendants
    var hideBelow = Infinity, kept = [];
    rs.forEach(function(r, i) {
      if (depth[i] <= hideBelow) hideBelow = Infinity;
      var hidden = !show[i] || depth[i] > hideBelow;
      r.style.display = hidden ? 'none' : '';
      if (show[i]) kept.push(i + 1);
      if (!hidden && r.classList.contains('collapsed')) hideBelow = depth[i];
    });
    tree.querySelector('.tax-status').textContent = q ?
      kept.length.toLocaleString() + ' of ' + rs.length.toLocaleString() + ' taxa' : rs.length.toLocaleString() + ' taxa';
    if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue('tax_tree_rows', q ? kept : null);
  }

  document.addEventListener('click', function(e) {
    var toggle = e.target.closest('.tax-tree .has-children > .tax-toggle');
    if (toggle) {
      toggle.parentNode.classList.toggle('collapsed');
      update(toggle.closest('.tax-tree'));
      return;
    }
    var btn = e.target.closest('.tax-tree [data-tax-action]');
    if (btn) {
      var tree = btn.closest('.tax-tree');
      // while searching, collapsing keeps the paths to matching taxa open
      rows(tree).forEach(function(r) {
        r.classList.toggle('collapsed', btn.dataset.taxAction === 'collapse' && r.classList.contains('has-children') &&
          !r.taxOnPath);
      });
      update(tree);
    }
  });

  // a new search starts with every branch expanded
  document.addEventListener('input', function(e) {
    if (!e.target.matches('.tax-tree .tax-search')) return;
    var tree = e.target.closest('.tax-tree');
    rows(tree).forEach(function(r) { r.classList.remove('collapsed'); });
    clearTimeout(tree.searchTimer);
    tree.searchTimer = setTimeout(function() { update(tree); }, 150);
  });
})();

if get(g:, 'myo_hs', 0)
  let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
  let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [{ 'name': 'myo', 'spec': 'stack:' . s:dir, 'dev': v:true, 'debug': v:true }]
endif

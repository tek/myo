if get(g:, 'myo_hs', 1)
  let s:dir = fnamemodify(expand('<sfile>'), ':p:h:h')
  let g:chromatin_rplugins = get(g:, 'chromatin_rplugins', []) + [{ 'name': 'myo', 'spec': 'stack:' . s:dir, 'dev': v:true, 'debug': get(g:, 'myo_debug', 0) }]
endif

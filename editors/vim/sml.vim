function! neoformat#formatters#sml#enabled() abort
    return ['smlformat']
    endfunction

function! neoformat#formatters#sml#smlformat() abort
    return {
           \ 'exe': 'smlformat',
           \ 'stdin': 1,
           \ 'args': ['-i']
           \ }
    endfunction

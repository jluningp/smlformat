function! neoformat#formatters#sml#enabled() abort
    return ['smlformat']
    endfunction

function! neoformat#formatters#sml#smlformat() abort
    return {
           \ 'exe': '/home/jeanne/smlformat/smlformat',
           \ 'stdin': 1,
           \ 'args': ['-i']
           \ }
    endfunction

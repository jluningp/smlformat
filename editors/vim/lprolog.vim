function! neoformat#formatters#lprolog#enabled() abort
    return ['smlformat']
    endfunction

function! neoformat#formatters#lprolog#smlformat() abort
    return {
           \ 'exe': '/home/jeanne/smlformat/smlformat',
           \ 'stdin': 1,
           \ 'args': ['-i']
           \ }
    endfunction

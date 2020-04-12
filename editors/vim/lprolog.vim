function! neoformat#formatters#lprolog#enabled() abort
    return ['smlformat']
    endfunction

function! neoformat#formatters#lprolog#smlformat() abort
    return {
           \ 'exe': 'smlformat',
           \ 'stdin': 1,
           \ }
    endfunction

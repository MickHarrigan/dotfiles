syntax on
set noshowmode
set autoindent 
set number
set modeline
set expandtab
set tabstop=4
set shiftwidth=4
colorscheme solarized

let g:ale_completion_enabled=1
let g:deoplete#enable_at_startup=1

" sets the tab selector to tab instead of the stock Control+n/Control+p
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

execute pathogen#infect()
filetype plugin indent on

" True Color

if (has('nvim'))
	let $NVIM_TUI_ENABLE_TRUE_COLOR = 1
    
endif

set background=dark

let g:airline_theme='solarized'

let g:solarized_termcolors=256


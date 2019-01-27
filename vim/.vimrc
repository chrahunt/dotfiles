" - XDG Directories -----------------------------------------------------------
if !empty($XDG_CACHE_HOME)
    let xdg_cache_home=$XDG_CACHE_HOME
else
    let xdg_cache_home=$HOME.'/.cache'
endif

if !isdirectory(xdg_cache_home.'/vim')
    call mkdir(xdg_cache_home.'/vim', 'p', 0700)
endif

" '//' ensures uniqueness
let &directory=xdg_cache_home.'/vim//'
let &backupdir=xdg_cache_home.'/vim//'

" - General -------------------------------------------------------------------
set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
filetype indent on
syntax on

set hidden
let mapleader = "-"
" for orgmode
let maplocalleader="-"

autocmd FuncUndefined * exe 'runtime autoload/' . expand('<afile>') . '.vim'
"# Misc Keybindings
nnoremap <Enter> i<Enter><Esc>l

"# Syntax highlighting
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
            \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
" highlights for folds
hi Folded ctermbg=DarkGrey

"# Split-screen
"## navigation
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

"## open location
set splitright
set splitbelow

"# Tabs
"## navigation
" mapping keycode to unused vim keycode
" avoids waiting compared to using <Esc>
nnoremap <Esc>[1;5I :tabnext<CR>
nnoremap <Esc>[1;6I :tabprevious<CR>

"# Filesystem navigation
" let :edit command use current directory when navigating with netrw
" http://vi.stackexchange.com/questions/631/make-edit-file-use-current-directory-during-explore
let g:netrw_keepdir=0

" quick paste toggle in normal and insert modes
set pastetoggle=<f5>
set showmode
" interface with X CLIPBOARD or windows clipboard
if has("clipboard")
    if has("win32")
        set clipboard=unnamed
    else
        if has("unix")
            set clipboard=unnamedplus
        endif
    endif
endif

" Folding
set foldmethod=syntax
set nofoldenable
set foldnestmax=10
set foldlevel=1
"" enable perl folding
let perl_fold=1
let perl_nofold_packages=1
let perl_fold_blocks=1

" - Filetypes -----------------------------------------------------------------

au BufNewFile,BufRead *.ipp set filetype=cpp

" 'syslog' is messages, always
au BufRead syslog set filetype=messages
au BufRead syslog.* set filetype=messages

au BufRead strace.* set filetype=strace

" Fast escape in visual block insert
set timeoutlen=1000 ttimeoutlen=0

" Force write, prompt for sudo
cmap w!! w !sudo tee > /dev/null %

" line
nmap <C-0> :let @+=expand("%") . ':' . line(".")<CR>

" long wrapped lines
set display+=lastline

" ctags
set autochdir
set tags=tags;
set csre

" compile/run
autocmd filetype cpp nnoremap <F4> :w <bar> exec '!g++ -std=c++14 -ggdb3 '.shellescape('%').' -o '.shellescape('%:r').' && ./'.shellescape('%:r')<CR>

" https://shapeshed.com/vim-templates/
augroup templates
    autocmd BufNewFile *.cpp 0r ~/.vim/templates/skeleton.cpp
augroup END

" - Plugins -------------------------------------------------------------------
" junegunn/vim-plug
call plug#begin('~/.local/share/vim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'rust-lang/rust.vim'
Plug 'idbrii/AsyncCommand'
Plug 'tpope/vim-surround'
Plug 'jceb/vim-orgmode'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --clang-completer' }

call plug#end()

" vim-easy-align mappings.
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
"autocmd filetype cpp nnoremap <F4> :w <bar> exec '!g++ -std=c++11 '.shellescape('%').' -o '.shellescape('%:r').' && ./'.shellescape('%:r')<CR>

" Workaround for vim-orgmode requiring the speeddating plugin
" https://github.com/jceb/vim-orgmode/issues/203
command -nargs=* -range SpeedDatingFormat

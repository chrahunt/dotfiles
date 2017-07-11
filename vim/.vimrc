set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
filetype indent on
syntax on

" Split-screen
"" navigation
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

"" open location
set splitbelow
set splitright

" Filesystem navigation
"" let :edit command use current directory when navigating with netrw
"" http://vi.stackexchange.com/questions/631/make-edit-file-use-current-directory-during-explore
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

" *.ipp files -> cpp
au BufNewFile,BufRead *.ipp set filetype=cpp
" 'syslog' is messages, always
au BufRead syslog set filetype=messages

" Fast escape in visual block insert
set timeoutlen=1000 ttimeoutlen=0

" In case I forget sudo
cmap w!! w !sudo tee > /dev/null %

" line
nmap <C-l> :let @+=expand("%") . ':' . line(".")<CR>

" ctags
set autochdir
set tags=tags;
set csre

" Plugins
" junegunn/vim-plug
"call plug#begin('~/.local/share/nvim/plugged')

"Plug 'junegunn/vim-easy-align'
"Plug 'rust-lang/rust.vim'

"call plug#end()

" vim-easy-align mappings.
" Start interactive EasyAlign in visual mode (e.g. vipga)
"xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
"nmap ga <Plug>(EasyAlign)

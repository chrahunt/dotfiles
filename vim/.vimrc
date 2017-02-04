set autoindent
set tabstop=4
set shiftwidth=4
set expandtab
filetype indent on

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

set textwidth=75

" quick paste toggle in normal and insert modes
set pastetoggle=<f5>
set showmode

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

" Plugins
" junegunn/vim-plug
call plug#begin('~/.local/share/nvim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'rust-lang/rust.vim'

call plug#end()

" vim-easy-align mappings.
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)


" - XDG Directories -----------------------------------------------------------
if !empty($XDG_CACHE_HOME)
    let xdg_cache_home=$XDG_CACHE_HOME.'/vim'
else
    let xdg_cache_home=$HOME.'/.cache/vim'
endif

if !isdirectory(xdg_cache_home)
    call mkdir(xdg_cache_home, 'p', 0700)
endif

if !empty($XDG_DATA_HOME)
    let xdg_data_home=$XDG_DATA_HOME.'/vim'
else
    let xdg_data_home=$HOME.'/.local/share/vim'
endif

if !isdirectory(xdg_data_home)
    call mkdir(xdg_data_home, 'p', 0700)
endif

" &option allows string variable usage, set does not.
" '//' ensures uniqueness
let &directory=xdg_cache_home.'//'
let &backupdir=xdg_cache_home.'//'
let &viminfo.=',n'.xdg_data_home.'/viminfo'

" - General -------------------------------------------------------------------
" Copy indent from current line when starting a new line
set autoindent
" Use 4 spaces by default for indentation
set tabstop=4
set shiftwidth=4
set expandtab
" Enable filetype detection and filetype-based autocmd execution
filetype on
" Set filetype-based indent by default
filetype indent on
" Load filetype-specific indent files from 'runtimepath'
syntax on

" Do not auto-wrap lines, ever
set textwidth=0

" If a buffer is loaded, just hide (don't unload) the one that was there
" before
set hidden

" Show col/lineno in the bottom-right
set ruler

" Allow backspace to erase autoindent, line breaks, and the start of
" the current insert. (legacy value may be '2' in other configs)
set backspace=indent,eol,start

" This key will be used in place of <leader> in other shortcuts.
let mapleader = " "

" Pipe character without gaps
let &fillchars = 'vert:' . nr2char(0x2502)
hi VertSplit cterm=NONE

autocmd FuncUndefined * exe 'runtime autoload/' . expand('<afile>') . '.vim'
"# Misc Keybindings
" TODO: Don't do in dired
"nnoremap <Enter> i<Enter><Esc>l

" Get the syntax attributes of the characters under the cursor
" (h)elp (d)escribe (h)ighlight
nmap <leader>hdh :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
            \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Use a muted color for fold highlights, which are usually really bad.
hi Folded ctermbg=DarkGrey cterm=bold

"# Split-screen
"## creation
" (s)plit
"   (v)ertical
nmap <leader>sv :vsp<Enter>
"   (h)orizontal
nmap <leader>sh :sp<Enter>

"## navigation
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

" (w)indow
"   (*) direction
nmap <leader>wh <C-w>h
nmap <leader>wj <C-w>j
nmap <leader>wk <C-w>k
nmap <leader>wl <C-w>l
nmap <leader>wc :q<Enter>

" When splitting, put new window to the right or below
set splitright
set splitbelow

" By default when pressing ESC, it may be interpreted as part
set timeoutlen=1000 ttimeoutlen=0

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
" As used in Boost
au BufNewFile,BufRead *.ipp set filetype=cpp

" For Spacemacs
au BufNewFile,BufRead .spacemacs set filetype=lisp

" 'syslog' is messages, always
au BufRead syslog set filetype=messages
au BufRead syslog.* set filetype=messages

au BufRead strace.* set filetype=strace

au FileType yaml setlocal tabstop=2 softtabstop=2 shiftwidth=2 expandtab

" Force write, prompt for sudo
cmap w!! w !sudo tee > /dev/null %

" Copy file/line to clipboard.
nnoremap <silent> <C-p> :let @+ = expand("%:p") . ':' . line(".")<CR>

" By default, a short line followed by a line that's too long for the rest of
" the screen will display like
" abc
" @
" @
" @
" This makes it display as much of the line as possible instead
set display=lastline

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

" TODO: Extract amazon-specific fixes
" Replace light blue status line text with dark text
hi StatusLine ctermfg=black

" - Plugins -------------------------------------------------------------------
" junegunn/vim-plug
call plug#begin('~/.local/share/vim/plugged')

Plug 'idbrii/AsyncCommand'
Plug 'jceb/vim-orgmode'
Plug 'junegunn/vim-easy-align'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'
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

" nerdtree config
map <C-n> :NERDTreeToggle<CR>
nmap <C-n> :NERDTreeToggle<CR>

" - Non-plugins ---------------------------------------------------------------
function! Hunter()
    " It takes 2 mins to fold 400k lines in my macbook air.
    function! ColBase()
        " determine column number (3rd col, code part) in the first (base) line
        call cursor(1, 1)
        let mypattern = '\v(.+:\d+\s+\w+\s+!\s+\zs)|(.+:\d+\s+\w+\s+\zs)'

        let [lnum, colbase] = searchpos(mypattern, 'n')
        return colbase
    endfunction

    function! Info(lnum)
        " determine the 3rd column position ( code part)
        call cursor(a:lnum, 1)
        let mypattern = '\v(.+:\d+\s+\w+\s+!\s+\zs)|(.+:\d+\s+\w+\s+\zs)'
        let [lnum, col_pos] = searchpos(mypattern, 'n')
        " dterminte the 3rd col indentation
        let indentlevel = (col_pos - w:colbase) / 3
        let info = [col_pos, indentlevel]
        return info
    endfunction

    function! GetPotionFold(lnum)
        let info = Info(a:lnum)
        let colcode = info[0]
        let indentlevel = info[1]
        let current_content = getline(a:lnum)
        if current_content[colcode-1: colcode+1] ==# '=> '
            let tmp = indentlevel + 1
            return '>' . tmp
        endif
        if current_content[colcode-1: colcode+1] ==# '<= '
            let tmp = indentlevel + 1
            return tmp
        endif
        return indentlevel
    endfunction

    let w:colbase = ColBase()
    set foldmethod=expr
    set foldexpr=GetPotionFold(v:lnum)

    " change fold default color
    highlight Folded ctermbg=Black ctermfg=darkgray

    " render 3rd column color
    function! HunterColor()
        syntax include @Python syntax/python.vim 
        syntax region pythonCode matchgroup=SnipPy start=/\v^.+:\d+\s+\w+\s+/ end=/$/ contains=@Python keepend
        hi SnipPy ctermfg=darkgray guifg=darkgray
        hi Snip1 ctermfg=darkred guifg=darkred gui=bold
        hi Snip2 ctermfg=lightred guifg=lightred
        match Snip1 /\v\<\=\s\w+:/
        2match Snip2 /\v\=\>\s\w+/
    endfunction
    call HunterColor()

    " enable click    
    " set mouse=a
    "set foldcolumn=8
endfunction

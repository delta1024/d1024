set relativenumber
syntax on
set runtimepath=/usr/share/vim/vimfiles,$VIMRUNTIME
filetype plugin indent on
set grepprg=grep\ -nH\ $*
let mapleader=" "
let g:tex_flavor = "latex"
let g:Tex_DefaultTargetFormat = 'pdf'
map <leader>fs :w<CR>
map <leader>wq :wq<CR>
map <leader>qq :q!<CR>
vnoremap <C-c> "+y
map <C-p> "+P
autocmd InsertEnter * norm zz
map <leader><leader> /<++><CR>4cl

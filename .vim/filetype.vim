if exists("did_load_filetypes")
  finish
endif
augroup filetypedetect
	au! BufNewFile,BufRead *.zu,*.zwt     setf zimbu
  au! BufNewFile,BufRead  *.ch,*.yac  setf cheat
augroup END

runtime! ftdetect/*.vim
au BufNewFile,BufRead *.mustache        setf mustache

" perl plugins


" node plugins


" python3 plugins
call remote#host#RegisterPlugin('python3', '/usr/share/vim/vimfiles/rplugin/python3/deoplete', [
      \ {'sync': v:false, 'name': '_deoplete_init', 'type': 'function', 'opts': {}},
     \ ])


" ruby plugins


" python plugins



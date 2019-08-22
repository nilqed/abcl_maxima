#

ln -s diffrat.lisp     diffrat.lsp 
ln -s disp1.lisp       disp1.lsp   
ln -s eval.lisp        eval.lsp    
ln -s eval2.lisp       eval2.lsp   
ln -s hcons.lisp       hcons.lsp   
#ln -s init.lisp        init.lsp
ln -s init2.lisp       init.lsp
ln -s init2.lisp       init2.lsp   
ln -s match.lisp       match.lsp   
ln -s match16.lisp     match16.lsp 
ln -s mma.lisp         mma.lsp     
ln -s parser.lisp      parser.lsp  
ln -s pf.lisp          pf.lsp      
ln -s poly.lisp        poly.lsp    
ln -s primeq.lisp      primeq.lsp  
ln -s rat1.lisp        rat1.lsp    
ln -s simp1.lisp       simp1.lsp   
ln -s stack1.lisp      stack1.lsp  
ln -s ucons1.lisp      ucons1.lsp  
ln -s uconsalt.lisp    uconsalt.lsp

patch < emardpatch.diff


" Vim syntax file
" " Language: mora
" " Maintainer: mtd
" " Latest Revision: September 2nd 2021

if exists("b:current_syntax")
    finish
endif


syn keyword basicKeywords if If then else Else For While fn op type data
syn keyword basicKeywords return dec case Case of cond Cond Init Update

syn keyword preProcKeywords module import op
syn match mrPreProc "#.*$"

syn match operators '\v[+\-/*^=<>@!$%&|:\\]'

syn match customInfixOp '[`][a-zA-Z_][a-zA-Z0-9_]*[`]'

syn match mrVar '[a-z_][a-zA-Z0-9_]*'
syn match mrType '[A-Z][a-zA-Z0-9_]*'

syn match mrNumber '\d\+'
syn match mrNumber '\d\+\.\d\+'

syn region mrBlockComment start="{-" end="-}" fold contains=mrTodo
"syn region mrBlock start="{" end="}" transparent fold 
"syn sync fromstart
"set foldmethod=syntax

syn match mrLineComment "--.*$" contains=mrTodo
syn keyword mrTodo contained TODO 



syn region mrString start='"' end='"'
syn region mrChar start=' \'\|^\'' end='\''


let b:current_syntax = "mora"

" The types are
" Todo
" Comment
" Statement
" Type
" PreProc
" Constant

hi def link mrLineComment       Comment
hi def link mrBlockComment      Comment
"hi def link mrPreProc           PreProc
hi def link preProcKeywords     PreProc
"hi def link mrBlock             Statement
hi def link mrType              Type
"hi def link mrVar               Todo
hi def link mrNumber            Constant
hi def link mrString            Constant
hi def link mrChar              Constant
hi def link mrTodo              Todo
hi def link basicKeywords       Statement
hi def link customInfixOp       Statement
hi def link operators           Statement

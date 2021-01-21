if exists('b:current_syntax') | finish | endif

syntax match term /= / nextgroup=termValue
syntax match termValue /.\+$/ contained
syntax match extra /\(\/\|#\) / nextgroup=extraValue
syntax match extraValue /.\+$/ contained
syntax match comment /' / nextgroup=commentValue
syntax match commentValue /.\+$/ contained
syntax match edge /\(--\|<>\|->\|<-\) / nextgroup=edgeValue
syntax match edgeValue /.\+$/ contained

hi def link term Statement
hi def link termValue Underlined
hi def link extra Constant
hi def link extraValue Comment
hi def link comment Statement
hi def link commentValue Statement
hi def link edge Constant
hi def link edgeValue Bold

let b:current_syntax = 'yaddl'

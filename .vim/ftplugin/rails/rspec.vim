if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim


exec "Snippet it it \"".st."description".et."\" do<CR>".st."content".et."<CR>end<CR>".st.et
exec "Snippet des describe \"".st."description".et."\" do<CR>".st."content".et."<CR>end<CR>".st.et

exec "Snippet rs response.should ".st.et
exec "Snippet rt render_template ".st.et

exec "Snippet sb .should_be ".st.et
exec "Snippet sn .should_not ".st.et

if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim



exec "Snippet cde context ".st."description".et." do<CR>".st."content".et."<CR>end<CR>".st.et
exec "Snippet sde setup do<CR>".st."content".et."<CR>end<CR>".st.et
exec "Snippet se should ".st."description".et."<CR>".st."content".et."<CR>end<CR>".st.et
exec "Snippet see should_eventually ".st."description".et."<CR>".st."content".et."<CR>end<CR>".st.et



exec "Snippet laf load_all_fixtures <CR>".st.et



exec "Snippet ac assert_contains(".st."collection".et.",".st."item".et.")<CR>".st.et
exec "Snippet anc assert_does_not_contains(".st."collection".et.",".st."item".et.")<CR>".st.et

exec "Snippet ane assert_did_not_send_email ".st.et
exec "Snippet ae assert_sent_email ".st.et

exec "Snippet ase assert_same_elements(".st."element1".et.",".st."element2".et.")<CR>".st.et
exec "Snippet as assert_save ".st.et

exec "Snippet av assert_valid(".st."obj".et.")<CR>".st.et

exec "Snippet axr assert_xml_response ".st.et

exec "Snippet rx request_xml ".st.et


exec "Snippet sa should_assign_to ".st.et
exec "Snippet sna should_not_assign_to ".st.et

exec "Snippet snsf should_not_set_the_flash ".st.et
exec "Snippet ssf should_set_the_flash_to ".st.et

exec "Snippet sr should_redirect_to \"".st."url".et."\"".st.et

exec "Snippet srf should_render_a_form ".st.et 
exec "Snippet srt should_render_template ".st.et

exec "Snippet srw should_respond_with ".st.et

exec "Snippet srwx should_respond_with_xml ".st.et


exec "Snippet sav should_allow_values_for ".st."attribute".et.",".st."good values".et
exec "Snippet snav should_not_allow_values_for ".st."attribute".et.",".st."bad values".et
exec "Snippet soanv should_only_allow_numeric_values_for ".st."attribute".et

exec "Snippet sbt should_belong_to :".st.et
exec "Snippet selat should_ensure_length_at_least ".st."element1".et.",".st."element2".et
exec "Snippet selir should_ensure_length_in_range ".st."element1".et.",(".st."range".et.")<CR>".st.et
exec "Snippet sevir should_ensure_value_in_range ".st."element1".et.",(".st."range".et.")<CR>".st.et
exec "Snippet shbtm should_have_and_belong_to_many :".st.et
exec "Snippet shcm should_have_class_methods ".st.et
exec "Snippet shdc should_have_db_column ".st."name".et.",".st."opts".et
exec "Snippet shdcs should_have_db_columns ".st."columns".et
exec "Snippet shi should_have_indices ".st."columns".et
exec "Snippet shim should_have_instance_methods ".st.et
exec "Snippet shm should_have_many :".st.et
exec "Snippet sho should_have_one :".st.et

exec "Snippet spa should_protect_attributes ".st."attribute".et
exec "Snippet srao should_require_acceptance_of ".st."attribute".et

exec "Snippet sra should_require_attributes ".st."attribute".et
exec "Snippet srua should_require_unique_attributes ".st."attribute".et



exec "Snippet sbr should_be_restful do |resource|<CR>".st."content".et."<CR>end<CR>".st.et

exec "Snippet ri resource.identifier =".st.":id".et."<CR>".st.et
exec "Snippet rk resource.klass =".st."Klass".et."<CR>".st.et
exec "Snippet ro resource.object =".st."object".et."<CR>".st.et
exec "Snippet rp resource.parent =".st."parent".et."<CR>".st.et
exec "Snippet ra resource.actions = [".st."actions".et."]<CR>".st.et
exec "Snippet rf resource.formats = [".st."formats".et."]<CR>".st.et
exec "Snippet rd resource.denied ".st."element".et

exec "Snippet rcp resource.create.params = {".st."params".et."}<CR>".st.et
exec "Snippet rup resource.update.params = {".st."params".et."}<CR>".st.et

exec "Snippet rcr resource.create.redirect =".st."url".et."<CR>".st.et
exec "Snippet rur resource.update.redirect =".st."url".et."<CR>".st.et
exec "Snippet rdr resource.destroy.redirect =".st."url".et."<CR>".st.et

exec "Snippet rcf resource.create.flash =".st."message".et
exec "Snippet ruf resource.update.flash =".st."message".et
exec "Snippet rdf resource.destroy.flash =".st."message".et

exec "Snippet rda resource.denied.actions = [".st."actions".et."]<CR>".st.et
exec "Snippet rdf resource.denied.flash =".st."message".et
exec "Snippet rdr resource.denied.redirect =".st."url".et




syntax keyword rubyRailsTestMethod  should_allow_values_for should_belong_to  should_eventually should_ensure_length_at_least should_ensure_length_in_range should_ensure_value_in_range should_have_and_belong_to_many  should_have_db_column should_have_db_columns should_have_index should_have_indices should_have_instance_methods should_have_many should_have_one should_not_allow_values_for should_only_allow_numeric_values_for should_protect_attributes should_require_acceptance_of should_require_attributes should_require_unique_attributes

syntax keyword rubyRailsTestMethod should_be_restful should_assign_to should_not_assign_to should_not_set_the_flash should_redirect_to should_render_a_form should_render_template should_respond_with should_set_the_flash_to assert_xml_response request_xml should_respond_with_xml should_respond_with_xml_for 

syntax keyword rubyRailsTestMethod assert_contains assert_did_not_send_email assert_does_not_contain assert_same_elements assert_save assert_sent_email assert_valid pretty_error_messages report! load_all_fixtures 

syntax keyword rubyDefine context setup should 

let maplocalleader=";"

"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Ruby snippets
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>atr <c-r>=IMAP_PutTextWithMovement("attr_reader :<+ reader name +>")<cr>
inoremap <buffer><localleader>atw <c-r>=IMAP_PutTextWithMovement("attr_writer :<+ writer name +>")<cr>
inoremap <buffer><localleader>bl <c-r>=IMAP_PutTextWithMovement("{ \|<+ arg +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>bldo <c-r>=IMAP_PutTextWithMovement("do \|<+ arg +>\|\n <+ block +>\nend")<cr>
inoremap <buffer><localleader>cas <c-r>=IMAP_PutTextWithMovement("case\nwhen <+ condition +>\n<+ action +>\nelse\n<+ action +>\nend")<cr>
inoremap <buffer><localleader>cast <c-r>=IMAP_PutTextWithMovement("case <+ target +>\nwhen <+ comparison +>\n<+ action +>\nelse\n<+ action +>\nend")<cr>
inoremap <buffer><localleader>con <c-r>=IMAP_PutTextWithMovement("concat( <+ other_array +> )")<cr>
inoremap <buffer><localleader>def <c-r>=IMAP_PutTextWithMovement("def <+ defname +>\nend")<cr>
inoremap <buffer><localleader>defi <c-r>=IMAP_PutTextWithMovement("def initialize\n<++>\nend")<cr>
inoremap <buffer><localleader>defs <c-r>=IMAP_PutTextWithMovement("def self.<+ class method name +>\nend")<cr>

inoremap <buffer><localleader>do <c-r>=IMAP_PutTextWithMovement("do \|<++>\|\n<++>\nend")<cr>
inoremap <buffer><localleader>if <c-r>=IMAP_PutTextWithMovement("if <+ boolean exp +>\n<+ block +>\nend")<cr>
inoremap <buffer><localleader>tif <c-r>=IMAP_PutTextWithMovement("<+ boolean exp +> ? <+ exp if true +> : <+ exp if false +>")<cr>
inoremap <buffer><localleader>elsif <c-r>=IMAP_PutTextWithMovement("elsif <+ boolean exp +>\n<+ block +>")<cr>
inoremap <buffer><localleader>unl <c-r>=IMAP_PutTextWithMovement("unless <+ boolean exp +>\n<+ block +>\nend")<cr>
inoremap <buffer><localleader>scan <c-r>=IMAP_PutTextWithMovement('scan(<+ exp +>)')<cr>


"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" WHERE TO PUT THESE
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>ali <c-r>=IMAP_PutTextWithMovement("alias <+ new +> <+ old +>")<cr>
inoremap <buffer><localleader>procnew <c-r>=IMAP_PutTextWithMovement("Proc.new { <+ block +> }")<cr>
inoremap <buffer><localleader>lam <c-r>=IMAP_PutTextWithMovement("lambda { <+ block +> }")<cr>
inoremap <buffer><localleader>beg <c-r>=IMAP_PutTextWithMovement("BEGIN {\n<+ code to run while file loading +>\n}")<cr>
inoremap <buffer><localleader>end <c-r>=IMAP_PutTextWithMovement("END {\n<+ code to run after execution finished +>\n}")<cr>


"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Strings
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>sqs <c-r>=IMAP_PutTextWithMovement("%q/<+ single quoted string +>/")<cr>
inoremap <buffer><localleader>dqs <c-r>=IMAP_PutTextWithMovement("%Q/<+ double quoted string +>/")<cr>
inoremap <buffer><localleader>int <c-r>=IMAP_PutTextWithMovement("#{<+ string to substitute +>}")<cr>


"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Arrays
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>%w <c-r>=IMAP_PutTextWithMovement("%w/<+ words in array (not substituted) +>/")<cr>
inoremap <buffer><localleader>%W <c-r>=IMAP_PutTextWithMovement("%W/<+ words in array (substituted) +>/")<cr>



"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Exceptions
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=



"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Classes
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>cl <c-r>=IMAP_PutTextWithMovement("class <+ class name +>\nend")<cr>
inoremap <buffer><localleader>cli <c-r>=IMAP_PutTextWithMovement("class <+class name +>\ndef initialize\n<++>\nend\nend")<cr>
inoremap <buffer><localleader>subcl <c-r>=IMAP_PutTextWithMovement("class <+ class name +> < <+ parent +>\nend")<cr>
inoremap <buffer><localleader>clstr <c-r>=IMAP_PutTextWithMovement("class <+ class name +> < Struct.new(:<+ arg_to_constructor +>)\ndef initialize(*args)\nsuper\nend\nend")<cr>
" Blankslate?
" Delegation?
" self?



"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Iterators
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Arrays

" Counting
inoremap <buffer><localleader>dow <c-r>=IMAP_PutTextWithMovement('downto(<+ lbound +>) { \|<+ arg +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>ste <c-r>=IMAP_PutTextWithMovement('step(<+ count +>) { \|<+ arg +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>tim <c-r>=IMAP_PutTextWithMovement('times { \|<+ arg +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>upt <c-r>=IMAP_PutTextWithMovement('upto(<+ ubound +>) { \|<+ arg +>\| <+ block +> }')<cr>

" Each Element
inoremap <buffer><localleader>ea <c-r>=IMAP_PutTextWithMovement("each {\|<+ record +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eai <c-r>=IMAP_PutTextWithMovement("each_index {\|<+ index +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eak <c-r>=IMAP_PutTextWithMovement("each_key {\|<+ key +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eal <c-r>=IMAP_PutTextWithMovement("each_line {\|<+ line +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eav <c-r>=IMAP_PutTextWithMovement("each_value {\|<+ value +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>reve <c-r>=IMAP_PutTextWithMovement("reverse_each {\|<+ e +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eap <c-r>=IMAP_PutTextWithMovement("each_pair {\|<+ name +>, <+ value +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eab <c-r>=IMAP_PutTextWithMovement("each_byte {\|<+ byte +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eac <c-r>=IMAP_PutTextWithMovement("each_char {\|<+ char +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eacon <c-r>=IMAP_PutTextWithMovement("each_cons(<+ window size +>) {\|<+ cons +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>easli <c-r>=IMAP_PutTextWithMovement("each_slice(<+ slice size +>) {\|<+ slice +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>eawi <c-r>=IMAP_PutTextWithMovement("each_with_index {\|<+ record +>, <+ index +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>map <c-r>=IMAP_PutTextWithMovement('map { \|<+ arg +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>mapwi <c-r>=IMAP_PutTextWithMovement('map_with_index { \|<+ arg +>, <+ index +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>inj <c-r>=IMAP_PutTextWithMovement('inject { \|<+ accumulator +>, <+ object +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>inj0 <c-r>=IMAP_PutTextWithMovement('inject(0) { \|<+ accumulator +>, <+ object +>\| <+ block +> }')<cr>
inoremap <buffer><localleader>inji <c-r>=IMAP_PutTextWithMovement('inject(<+ initial +>) { \|<+ accumulator +>, <+ object +>\| <+ block +> }')<cr>


" Files
inoremap <buffer><localleader>dirg <c-r>=IMAP_PutTextWithMovement("Dir.glob(\"<+ pattern +>\") { \|file\| <+ block +> }")<cr>
inoremap <buffer><localleader>file <c-r>=IMAP_PutTextWithMovement("File.foreach(\"<+ dirname +>\") { \|file\| <+ block +> }")<cr>
inoremap <buffer><localleader>open <c-r>=IMAP_PutTextWithMovement("open(\"<+ path or url +>\", \"<+ rw +>\") { \|io\| <+ block +> }")<cr>


" Ordering
inoremap <buffer><localleader>sor  <c-r>=IMAP_PutTextWithMovement("sort {\|<+ el1 +>, <+ el2 +>\| <+el2 +> <=> <+ el1 +> }")<cr>
inoremap <buffer><localleader>sorb <c-r>=IMAP_PutTextWithMovement("sort_by { \|<+ arg +>\| <+ block +> }")<cr>


" Searching and Selection
inoremap <buffer><localleader>all <c-r>=IMAP_PutTextWithMovement("all? {\|<+ element +>\| <+ condition +> }")<cr>
inoremap <buffer><localleader>any <c-r>=IMAP_PutTextWithMovement("any? {\|<+ element +>\| <+ condition +> }")<cr>
inoremap <buffer><localleader>cfy <c-r>=IMAP_PutTextWithMovement("classify {\|<+ element +>\| <+ condition +> }")<cr>
inoremap <buffer><localleader>col <c-r>=IMAP_PutTextWithMovement("collect { \|<+ obj +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>col! <c-r>=IMAP_PutTextWithMovement("collect! { \|<+ obj +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>det <c-r>=IMAP_PutTextWithMovement("detect { \|<+ obj +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>detprc <c-r>=IMAP_PutTextWithMovement("detect(<+ proc/lambda +>) { \|<+ obj +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>fet <c-r>=IMAP_PutTextWithMovement("fetch(<+ name +>) { \|<+ key +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>fin <c-r>=IMAP_PutTextWithMovement("find { \|<+ element +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>fina <c-r>=IMAP_PutTextWithMovement("find_all { \|<+ element +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>grep <c-r>=IMAP_PutTextWithMovement("grep(/<+ pattern +>/) { \|<+ match +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>max <c-r>=IMAP_PutTextWithMovement("max {\|<+ element1 +>, <+ element2 +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>min <c-r>=IMAP_PutTextWithMovement("min {\|<+ element1 +>, <+ element2 +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>par <c-r>=IMAP_PutTextWithMovement("partition {\|<+ element +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>rej <c-r>=IMAP_PutTextWithMovement("reject {\|<+ element +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>sel <c-r>=IMAP_PutTextWithMovement("select {\|<+ element +>\| <+ block +> }")<cr>


" Strings
inoremap <buffer><localleader>sub <c-r>=IMAP_PutTextWithMovement("sub(/<+ pattern +>/) {\|<+ match +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>gsub <c-r>=IMAP_PutTextWithMovement("gsub(/<+ pattern +>/) {\|<+ match +>\| <+ block +> }")<cr>
inoremap <buffer><localleader>scan <c-r>=IMAP_PutTextWithMovement("scan(/<+ pattern +>/) {\|<+ match +>\| <+ block +> }")<cr>



"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" Unit testing
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>tc <c-r>=IMAP_PutTextWithMovement("require \"test/unit\"\nrequire \"<+ lib file name +>\"\n\nclass Test<+ NameOfTestCases +> < Test::Unit:TestCase\ndef test_<+ test case name +>\n<++>\nend\nend")<cr>
inoremap <buffer><localleader>deft <c-r>=IMAP_PutTextWithMovement("def test_<+ case name +>\nend")<cr>

" assertions
inoremap <buffer><localleader>ass <c-r>=IMAP_PutTextWithMovement('assert(<+ boolean condition [, message] +>)')<cr>

inoremap <buffer><localleader>ani <c-r>=IMAP_PutTextWithMovement('assert_nil(<+ object [, message] +>)')<cr>
inoremap <buffer><localleader>ann <c-r>=IMAP_PutTextWithMovement('assert_not_nil(<+ object [, message] +>)')<cr>

inoremap <buffer><localleader>aeq <c-r>=IMAP_PutTextWithMovement('assert_equal(<+ expected +>, <+ actual [, message] +>)')<cr>
inoremap <buffer><localleader>ane <c-r>=IMAP_PutTextWithMovement('assert_not_equal(<+ expected +>, <+ actual [, message] +>)')<cr>

inoremap <buffer><localleader>aid <c-r>=IMAP_PutTextWithMovement('assert_in_delta(<+ expected float +>, <+ actual float +>, <+ delta [, message] +>)')<cr>

inoremap <buffer><localleader>ara <c-r>=IMAP_PutTextWithMovement('assert_raise(<+ exception +>) { <+ block +> }')<cr>
inoremap <buffer><localleader>anr <c-r>=IMAP_PutTextWithMovement('assert_nothing_raised(<+ exception +>) { <+ block +> }')<cr>

inoremap <buffer><localleader>aio <c-r>=IMAP_PutTextWithMovement('assert_instance_of(<+ class +>, <+ object to compare [, message] +>)')<cr>
inoremap <buffer><localleader>ako <c-r>=IMAP_PutTextWithMovement('assert_kind_of(<+ class +>, <+ object to compare [, message] +>)')<cr>

inoremap <buffer><localleader>art <c-r>=IMAP_PutTextWithMovement('assert_respond_to(<+ object +>, <+ resp. to this message [, message] +>)')<cr>

inoremap <buffer><localleader>ama <c-r>=IMAP_PutTextWithMovement('assert_match(/<+ regexp +>/<+ flags +>, <+ string [, message] +>)')<cr>
inoremap <buffer><localleader>anm <c-r>=IMAP_PutTextWithMovement('assert_no_match(/<+ regexp +>/<+ flags +>, <+ string [, message] +>)')<cr>

inoremap <buffer><localleader>asa <c-r>=IMAP_PutTextWithMovement('assert_same(<+ expected +>, <+ actual [, message] +>)')<cr>
inoremap <buffer><localleader>ans <c-r>=IMAP_PutTextWithMovement('assert_not_same(<+ expected +>, <+ actual [, message] +>)')<cr>

inoremap <buffer><localleader>aop <c-r>=IMAP_PutTextWithMovement('assert_operator(<+ obj1 +>, <+ operator +>, <+ obj2 [, message] +>)')<cr>

inoremap <buffer><localleader>ath <c-r>=IMAP_PutTextWithMovement('assert_throws(<+ expected symbol [, message] +>) { <+ block +> }')<cr>
inoremap <buffer><localleader>ase <c-r>=IMAP_PutTextWithMovement('assert_send(<+ send array (send array[1] to [0] with [x] as args; exp true). [, message] +>)')<cr>


"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" wrap visual selection
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
vnoremap <buffer><localleader>[ <C-C>`>a]<Esc>`<i[<Esc>
vnoremap <buffer><localleader>( <C-C>`>a)<Esc>`<i(<Esc>
vnoremap <buffer><localleader>{ <C-C>`>a}<Esc>`<i{<Esc>
vnoremap <buffer><localleader>" <C-C>`>a"<Esc>`<i"<Esc>
vnoremap <buffer><localleader>` <C-C>`>a`<Esc>`<i`<Esc>


"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
" misc
"=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
inoremap <buffer><localleader>h <Space>=><Space>
inoremap <buffer><localleader>com <Space><=><Space>

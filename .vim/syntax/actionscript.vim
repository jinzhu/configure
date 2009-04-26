" Vim syntax file
" Language:	actionScript
" Maintainer:	Abdul Qabiz (mail at abdulqabiz.com)
" 		Author and previous maintainer:
" 		Igor Dvorsky <amigo@modesite.net>
" URL:		http://www.abdulqabiz.com/files/vim/actionscript.vim
" Original URL: http://www.modesite.net/vim/actionscript.vim
" Last Change:	2006 Jun 20
" Updated to support AS 2.0 2004 Mar 12 by Richard Leider  (richard@appliedrhetoric.com)
" Updated to support new AS 2.0 Flash 8 Language Elements 2005 September 29 (richard@appliedrhetoric.com)
" Updated to support new AS 3.0 Language Elements 2006 June 20 by Abdul Qabiz (mail at abdulqabiz.com)


" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
  finish
endif
  let main_syntax = 'actionscript'
endif

" based on "JavaScript" VIM syntax by Claudio Fleiner <claudio@fleiner.com>

syn case ignore
syn match   actionScriptLineComment			"\/\/.*$"
syn match   actionScriptCommentSkip			"^[ \t]*\*\($\|[ \t]\+\)"
syn region  actionScriptCommentString			start=+"+  skip=+\\\\\|\\"+  end=+"+ end=+\*/+me=s-1,he=s-1 contains=actionScriptSpecial,actionScriptCommentSkip,@htmlPreproc
syn region  actionScriptComment2String			start=+"+  skip=+\\\\\|\\"+  end=+$\|"+  contains=actionScriptSpecial,@htmlPreproc
syn region  actionScriptComment				start="/\*"  end="\*/" contains=actionScriptCommentString,actionScriptCharacter,actionScriptNumber
syn match   actionScriptSpecial				"\\\d\d\d\|\\."
syn region  actionScriptStringD				start=+"+  skip=+\\\\\|\\"+  end=+"+  contains=actionScriptSpecial,@htmlPreproc
syn region  actionScriptStringS				start=+'+  skip=+\\\\\|\\'+  end=+'+  contains=actionScriptSpecial,@htmlPreproc
syn match   actionScriptSpecialCharacter		"'\\.'"
syn match   actionScriptNumber				"-\=\<\d\+L\=\>\|0[xX][0-9a-fA-F]\+\>"
syn keyword actionScriptConditional			if else and or not
syn keyword actionScriptRepeat				do while for in
syn keyword actionScriptCase				break continue switch case default
syn keyword actionScriptConstructor			new
syn keyword actionScriptObjects				arguments Array Boolean Date _global Math Number Object String super var this Accessibility Color Key _level Mouse _root Selection Sound Stage System TextFormat LoadVars XML XMLSocket XMLNode LoadVars Button TextField TextSnapshot CustomActions Error ContextMenu ContextMenuItem NetConnection NetStream Video PrintJob MovieClipLoader StyleSheet Camera LocalConnection Microphone SharedObject MovieClip
syn keyword actionScriptStatement			return with
syn keyword actionScriptFunction			function on onClipEvent
syn keyword actionScriptValue				true false undefined null NaN void
syn keyword actionScriptArray				concat join length pop push reverse shift slice sort sortOn splice toString unshift
syn keyword actionScriptDate				getDate getDay getFullYear getHours getMilliseconds getMinutes getMonth getSeconds getTime getTimezoneOffset getUTCDate getUTCDay getUTCFullYear getUTCHours getUTCMilliseconds getUTCMinutes getUTCMonth getUTCSeconds getYear setDate setFullYear setHours setMilliseconds setMinutes setMonth setSeconds setTime setUTCDate setUTCFullYear setUTCHours setUTCMilliseconds setUTCMinutes setUTCMonth setUTCSeconds setYear UTC 
syn keyword actionScriptMath				abs acos asin atan atan2 ceil cos E exp floor log LOG2E LOG10E LN2 LN10 max min PI pow random round sin sqrt SQRT1_2 SQRT2 tan -Infinity Infinity
syn keyword actionScriptNumberObj			MAX_VALUE MIN_VALUE NaN NEGATIVE_INFINITY POSITIVE_INFINITY valueOf 
syn keyword actionScriptObject				addProperty __proto__ registerClass toString unwatch valueOf watch
syn keyword actionScriptString				charAt charCodeAt concat fromCharCode indexOf lastIndexOf length slice split substr substring toLowerCase toUpperCase add le lt gt ge eq ne chr mbchr mblength mbord mbsubstring ord
syn keyword actionScriptColor				getRGB getTransform setRGB setTransform
syn keyword actionScriptKey					addListener BACKSPACE CAPSLOCK CONTROL DELETEKEY DOWN END ENTER ESCAPE getAscii getCode HOME INSERT isDown isToggled LEFT onKeyDown onKeyUp PGDN PGUP removeListener RIGHT SHIFT SPACE TAB UP ALT
syn keyword actionScriptMouse				hide onMouseDown onMouseUp onMouseMove show onMouseWheel
syn keyword actionScriptSelection			getBeginIndex getCaretIndex getEndIndex getFocus setFocus setSelection	
syn keyword actionScriptSound				attachSound duration getBytesLoaded getBytesTotal getPan getTransform getVolume loadSound onLoad onSoundComplete position setPan setTransform setVolume start stop onID3
syn keyword actionScriptStage				align height onResize scaleMode width
syn keyword actionScriptSystem				capabilities hasAudioEncoder hasAccessibility hasAudio hasMP3 language manufacturer os pixelAspectRatio screenColor screenDPI screenResolution.x screenResolution.y version hasVideoEncoder security useCodepage exactSettings hasEmbeddedVideo screenResolutionX screenResolutionY input isDebugger serverString hasPrinting playertype hasStreamingAudio hasScreenBroadcast hasScreenPlayback hasStreamingVideo avHardwareDisable localFileReadDisable windowlesDisable
syn keyword actionScriptTextFormat			align blockIndent bold bullet color font getTextExtent indent italic leading leftMargin rightMargin size tabStops target underline url	
syn keyword actionScriptCommunication		contentType getBytesLoaded getBytesTotal load loaded onLoad send sendAndLoad toString	addRequestHeader fscommand MMExecute
syn keyword actionScriptXMLSocket			close connect onClose onConnect onData onXML
syn keyword actionScriptTextField			autoSize background backgroundColor border borderColor bottomScroll embedFonts _focusrect getDepth getFontList getNewTextFormat getTextFormat hscroll html htmlText maxChars maxhscroll maxscroll multiline onChanged onScroller onSetFocus _parent password _quality removeTextField replaceSel replaceText restrict selectable setNewTextFormat setTextFormat text textColor textHeight textWidth type variable wordWrap condenseWhite mouseWheelEnabled textFieldHeight textFieldWidth ascent descent
syn keyword actionScriptMethods				callee caller _alpha attachMovie beginFill beginGradientFill clear createEmptyMovieClip createTextField _currentframe curveTo _droptarget duplicateMovieClip enabled endFill focusEnabled _framesloaded getBounds globalToLocal gotoAndPlay gotoAndStop _height _highquality hitArea hitTest lineStyle lineTo loadMovie loadMovieNum loadVariables loadVariablesNum localToGlobal moveTo _name nextFrame onDragOut onDragOver onEnterFrame onKeyDown onKeyUp onKillFocus onMouseDown onMouseMove onMouseUp onPress onRelease onReleaseOutside onRollOut onRollOver onUnload play prevFrame removeMovieClip _rotation setMask _soundbuftime startDrag stopDrag swapDepths tabChildren tabIndex _target _totalframes trackAsMenu unloadMovie unloadMovieNum updateAfterEvent _url useHandCursor _visible _width _x _xmouse _xscale _y _ymouse _yscale tabEnabled asfunction call setInterval clearInterval setProperty stopAllSounds #initclip #endinitclip delete unescape escape eval apply prototype getProperty getTimer getURL getVersion ifFrameLoaded #include instanceof int new nextScene parseFloat parseInt prevScene print printAsBitmap printAsBitmapNum printNum scroll set targetPath tellTarget toggleHighQuality trace typeof isActive getInstanceAtDepth getNextHighestDepth getNextDepth getSWFVersion getTextSnapshot isFinite isNAN updateProperties _lockroot get install list uninstall showMenu onSelect builtInItems save zoom quality loop rewind forward_back customItems caption separatorBefore visible attachVideo bufferLength bufferTime currentFps onStatus pause seek setBuffertime smoothing time bytesLoaded bytesTotal addPage paperWidth paperHeight pageWidth pageHeight orientation loadClip unloadClip getProgress onLoadStart onLoadProgress onLoadComplete onLoadInit onLoadError styleSheet copy hideBuiltInItem transform activityLevel allowDomain allowInsecureDomain attachAudio bandwidth deblocking domain flush fps gain getLocal getRemote getSize index isConnected keyFrameInterval liveDelay loopback motionLevel motionTimeOut menu muted names onActivity onSync publish rate receiveAudio receiveVideo setFps setGain setKeyFrameInterval setLoopback setMode setMotionLevel setQuality setRate setSilenceLevel setUseEchoSuppression showSettings setClipboard silenceLevel silenceTimeOut useEchoSuppression
syn match   actionScriptBraces				"([{}])"
syn keyword actionScriptException 			try catch finally throw name message
syn keyword actionScriptXML					attributes childNodes cloneNode createElement createTextNode docTypeDecl status firstChild hasChildNodes lastChild insertBefore nextSibling nodeName nodeType nodeValue parentNode parseXML previousSibling removeNode xmlDecl ignoreWhite
syn keyword actionScriptArrayConstant 		CASEINSENSITIVE DESCENDING UNIQUESORT RETURNINDEXEDARRAY NUMERIC
syn keyword actionScriptStringConstant 		newline
syn keyword actionScriptEventConstant 		press release releaseOutside rollOver rollOut dragOver dragOut enterFrame unload mouseMove mouseDown mouseUp keyDown keyUp data
syn keyword actionScriptTextSnapshot 		getCount setSelected getSelected getText getSelectedText hitTestTextNearPos findText setSelectColor
syn keyword actionScriptID3 				id3 artist album songtitle year genre track comment COMM TALB TBPM TCOM TCON TCOP TDAT TDLY TENC TEXT TFLT TIME TIT1 TIT2 TIT3 TKEY TLAN TLEN TMED TOAL TOFN TOLY TOPE TORY TOWN TPE1 TPE2 TPE3 TPE4 TPOS TPUB TRCK TRDA TRSN TRSO TSIZ TSRX TSSE TYER WXXX
syn keyword actionScriptAS2 				class extends public private static interface implements import dynamic
syn keyword actionScriptStyleSheet 			parse parseCSS getStyle setStyle getStyleNames
syn keyword flash8Functions                             onMetaData onCuePoint flashdisplay flashexternal flashfilters flashgeom flashnet flashtext addCallback applyFilter browse cancel clone colorTransform  containsPoint containsRectangle copyChannel copyPixels createBox createGradientBox deltaTransformPoint dispose download draw equals fillRect floodFill generateFilterRect getColorBoundsRect getPixel getPixel32 identity inflate inflatePoint interpolate intersection intersects invert isEmpty loadBitmap merge noise normalize offsetPoint paletteMap perlinNoise pixelDissolve polar rotate scale setAdvancedAntialiasingTable setEmpty setPixel setPixel32 subtract threshold transformPoint translate union upload
syn keyword flash8Constants  				ALPHANUMERIC_FULL ALPHANUMERIC_HALF CHINESE JAPANESE_HIRAGANA JAPANESE_KATAKANA_FULL JAPANESE_KATAKANA_HALF KOREAN UNKNOWN
syn keyword flash8Properties 				appendChild cacheAsBitmap opaqueBackground scrollRect keyPress #initclip #endinitclip kerning letterSpacing onHTTPStatus lineGradientStyle IME windowlessDisable hasIME hideBuiltInItems onIMEComposition getEnabled setEnabled getConversionMode setConversionMode setCompositionString doConversion idMap antiAliasType available bottom bottomRight concatenatedColorTransform concatenatedMatrix creationDate creator fileList maxLevel modificationDate pixelBounds rectangle rgb top topLeft attachBitmap beginBitmapFill blendMode filters getRect scale9Grid gridFitType sharpness thickness
syn keyword flash8Classes 				BevelFilter BitmapData BitmapFilter BlurFilter ColorMatrixFilter ColorTransform ConvolutionFilter DisplacementMapFilter DropShadowFilter ExternalInterface FileReference FileReferenceList GlowFilter GradientBevelFilter GradientGlowFilter Matrix Point Rectangle TextRenderer
syn keyword actionScriptInclude #include #initClip #endInitClip include
syn keyword actionScriptAS3 				class extends public private static interface implements import dynamic protected
syn keyword flash9Constants				Infinity -Infinity NaN undefined
syn keyword flash9Functions				Array Boolean decodeURI decodeURIComponent encodeURI encodeURIComponent escape int isFinite isNaN isXMLName Number Object parseFloat parseInt String trace uint unescape XML XMLList
syn keyword flash9Classes			AbstractEvent AbstractInvoker AbstractMessage AbstractOperation AbstractService AbstractTarget Accessibility AccessibilityProperties AccessPrivileges AccessPrivilegesPrivilege Accordion AccordionHeader AccordionHeaderSkin AcknowledgeMessage ActionEffectInstance ActionScriptVersion ActivatorSkin ActivityEvent AddChild AddChildAction AddChildActionInstance Alert AMFChannel AnimateProperty AnimatePropertyInstance AntiAliasType Application ApplicationControlBar ApplicationDomain AreaChart AreaRenderer AreaSeries AreaSeriesItem AreaSeriesRenderData AreaSet ArgumentError arguments Array ArrayCollection ArrayUtil AssociationDefinition AsyncMessage AsyncRequest AsyncToken AVM1Movie AxisBase AxisLabel AxisLabelSet AxisRenderer Back BarChart BarSeries BarSeriesItem BarSeriesRenderData BarSet BaseListData BevelFilter BindingUtils Bitmap BitmapAsset BitmapData BitmapDataChannel BitmapFill BitmapFilter BitmapFilterQuality BitmapFilterType BlendMode Blur BlurFilter BlurInstance Boolean Border Bounce BoundedValue Box BoxDirection BoxItemRenderer BubbleChart BubbleSeries BubbleSeriesItem BubbleSeriesRenderData Button ButtonAsset ButtonBar ButtonBarButtonSkin ButtonLabelPlacement ButtonSkin ByteArray CalendarLayoutChangeEvent Camera CandlestickChart CandlestickItemRenderer CandlestickSeries Canvas Capabilities CapsStyle CartesianChart CartesianTransform CategoryAxis ChangeWatcher Channel ChannelError ChannelEvent ChannelFaultEvent ChannelSet ChartBase ChartElement ChartItem ChartItemEvent ChartLabel ChartState CheckBox CheckBoxIcon ChildExistenceChangedEvent CircleItemRenderer Circular Class ClassFactory ClientInputError CloseEvent CollectionEvent CollectionEventKind CollectionViewError ColorMatrixFilter ColorPicker ColorPickerEvent ColorPickerSkin ColorTransform ColorUtil ColumnChart ColumnSeries ColumnSeriesItem ColumnSeriesRenderData ColumnSet ComboBase ComboBox ComboBoxArrowSkin CommandMessage ComponentDescriptor CompositeEffect CompositeEffectInstance ConcreteDataService Conflict ConflictDetector Conflicts Consumer Container ContainerCreationPolicy ContextMenu ContextMenuBuiltInItems ContextMenuEvent ContextMenuItem ControlBar ConvolutionFilter CreditCardValidator CreditCardValidatorCardType CrossItemRenderer CSMSettings CSSStyleDeclaration Cubic CuePointEvent CuePointManager CurrencyFormatter CurrencyValidator CurrencyValidatorAlignSymbol CursorBookmark CursorError CursorEvent CursorManager CursorManagerPriority DataAssociationMessage DataConflictEvent DataConsumer DataDescription DataErrorMessage DataEvent DataGrid DataGridBase DataGridColumn DataGridColumnDropIndicator DataGridColumnResizeSkin DataGridDragProxy DataGridEvent DataGridEventReason DataGridHeaderSeparator DataGridItemRenderer DataGridListData DataGridSortArrow DataList DataListError DataService DataServiceError DataServiceFaultEvent DataStore DataTip DataTransform Date DateBase DateChooser DateChooserEvent DateChooserEventDetail DateChooserIndicator DateChooserMonthArrowSkin DateChooserYearArrowSkin DateField DateFormatter DateTimeAxis DateValidator DefaultDataDescriptor DeferredInstanceFromClass DeferredInstanceFromFunction DefinitionError DiamondItemRenderer Dictionary DirectHTTPChannel DisplacementMapFilter DisplacementMapFilterMode DisplayObject DisplayObjectContainer Dissolve DissolveInstance DividedBox DividerEvent DownloadProgressBar DragEvent DragManager DragSource DropdownEvent DropShadowFilter DualStyleObject DynamicEvent EdgeMetrics Effect EffectEvent EffectInstance EffectManager EffectTargetFilter Elastic EmailValidator EncodingError Endian EOFError Error ErrorEvent ErrorMessage EvalError Event EventDispatcher EventPhase EventPriority Exponential ExternalInterface Fade FadeInstance Fault FaultEvent FileFilter FileReference FileReferenceList FlexBitmap FlexEvent FlexMovieClip FlexPrintJob FlexPrintJobScaleType FlexShape FlexSimpleButton FlexSprite FlexTextField FocusEvent FocusManager Font FontStyle FontType Form Formatter FormHeading FormItem FormItemDirection FormItemLabel FrameLabel Function Glow GlowFilter GlowInstance GradientBase GradientBevelFilter GradientEntry GradientGlowFilter GradientType Graphics Grid GridFitType GridItem GridLines GridRow HaloBorder HaloColors HaloFocusRect HBox HDividedBox HeaderEvent HistoryManager HitData HLOCChart HLOCItemRenderer HLOCSeries HLOCSeriesBase HLOCSeriesItem HLOCSeriesRenderData HorizontalList HRule HScrollBar HSlider HTTPChannel HTTPRequestMessage HTTPService HTTPService HTTPStatusEvent IAccessPrivileges IAutomationObject IAutomationObjectContainer IAxis IBar IBitmapDrawable IChangeObject IChartElement IChildList ICollectionView IColumn IContainer ID3Info IDataInput IDataOutput IDataRenderer IDeferredInstance IDeferredInstantiationUIComponent IDropInListItemRenderer IDynamicPropertyOutput IDynamicPropertyWriter IEventDispatcher IExternalizable IFactory IFill IFlexAsset IFlexDisplayObject IFocusManager IFocusManagerComponent IFocusManagerContainer IFocusManagerGroup IHistoryManagerClient IIMESupport IInvalidating ILayoutManagerClient IList IListItemRenderer IllegalOperationError ILogger ILoggingTarget Image IManaged IME IMEConversionMode IMEEvent IMenuDataDescriptor IMenuItemRenderer IMessage IMXMLObject IMXMLSupport IndexChangedEvent InstanceCache int InteractiveObject InterpolationMethod InvalidCategoryError InvalidChannelError InvalidDestinationError InvalidFilterError InvalidSWFError IOError IOErrorEvent IOverride IPreloaderDisplay IPropertyChangeNotifier IRawChildrenContainer IRepeater IRepeaterClient IResponder Iris IrisInstance ISimpleStyleClient IStackable IStroke IStyleClient ISystemManager ItemClickEvent ItemPendingError ItemResponder IToolTip IToolTipManagerClient ITreeDataDescriptor IUIComponent IUID IViewCursor JointStyle Keyboard KeyboardEvent KeyLocation Label LayoutManager Legend LegendData LegendItem LegendMouseEvent Linear LinearAxis LinearGradient LinearGradientStroke LineChart LineFormattedTarget LineRenderer LineScaleMode LineSeries LineSeriesItem LineSeriesRenderData LineSeriesSegment LinkBar LinkButton LinkButtonSkin LinkSeparator List ListBase ListBaseSeekPending ListBaseSelectionData ListCollectionView ListData ListDropIndicator ListEvent ListEventReason ListItemDragProxy ListItemRenderer ListItemSelectEvent ListRowInfo Loader LoaderConfig LoaderContext LoaderInfo LoadEvent LocalConnection Locale Log LogAxis LogEvent LogEventLevel LogLogger MaskEffect MaskEffectInstance Math Matrix MBeanAttributeInfo MBeanFeatureInfo MBeanInfo MBeanOperationInfo MBeanParameterInfo MemoryError Menu MenuBar MenuBarBackgroundSkin MenuBarItem MenuEvent MenuItemRenderer MessageAckEvent MessageAgent MessageCacheItem MessageEvent MessageFaultEvent MessagePersister MessagePersisterEvent MessageResponder MessageSerializationError MessageStore MessageStoreEvent MessagingError Metadata MetadataEvent Microphone MiniDebugTarget MorphShape Mouse MouseEvent Move MoveEvent MoveInstance MovieClip MovieClipAsset MovieClipLoaderAsset Namespace NavBar NetConnection NetConnectionChannel NetStatusEvent NetStream NoChannelAvailableError NoDataAvailableError NoSuchChannelError Number NumberBase NumberBaseRoundType NumberFormatter NumberValidator NumericAxis NumericStepper NumericStepperDownSkin NumericStepperEvent NumericStepperUpSkin Object ObjectEncoding ObjectName ObjectProxy ObjectUtil Operation Operation Operation Operation PagedMessage Panel Parallel ParallelInstance Pause PauseInstance PhoneFormatter PhoneNumberValidator PieChart PieSeries PieSeriesItem PieSeriesRenderData PixelSnapping PlotChart PlotSeries PlotSeriesItem PlotSeriesRenderData Point PolarChart PolarTransform PollingChannel PopUpButton PopUpButtonSkin PopUpIcon PopUpManager PopUpManagerChildList PopUpMenuButton PopUpMenuIcon Preloader PrintDataGrid PrintJob PrintJobOptions PrintJobOrientation PriorityQueue Producer ProgrammaticSkin ProgressBar ProgressBarDirection ProgressBarLabelPlacement ProgressBarMode ProgressBarSkin ProgressEvent ProgressIndeterminateSkin ProgressTrackSkin PropertyChangeEvent PropertyChangeEventKind PropertyChanges Proxy QName Quadratic Quartic Quintic RadialGradient RadioButton RadioButtonGroup RadioButtonIcon RangeError Rectangle RectangularBorder RectangularDropShadow ReferenceError RegExp RegExpValidationResult RegExpValidator RemoteObject RemoteObject RemotingMessage RemoveChild RemoveChildAction RemoveChildActionInstance RenderData Repeater Resize ResizeEvent ResizeInstance ResourceBundle Responder ResultEvent RichTextEditor Rotate RotateInstance RoundedRectangle RPCMessage RSLEvent RTMPChannel Scene ScriptTimeoutError ScrollArrowSkin ScrollBar ScrollBarDirection ScrollControlBase ScrollEvent ScrollEventDetail ScrollEventDirection ScrollPolicy ScrollThumb ScrollThumbSkin ScrollTrackSkin SecureAMFChannel Security SecurityDomain SecurityError SecurityErrorEvent SecurityPanel Sequence SequencedMessage SequenceInstance Series SeriesEffect SeriesEffectInstance SeriesInterpolate SeriesInterpolateInstance SeriesSlide SeriesSlideInstance SeriesZoom SeriesZoomInstance SetEventHandler SetProperty SetPropertyAction SetPropertyActionInstance SetStyle SetStyleAction SetStyleActionInstance ShadowBoxItemRenderer ShadowLineRenderer Shape SharedObject SharedObjectFlushStatus SimpleButton SimpleXMLDecoder SimpleXMLEncoder Sine Slider SliderDataTip SliderDirection SliderEvent SliderEventClickTarget SliderHighlightSkin SliderLabel SliderThumb SliderThumbSkin SliderTrackSkin SOAPHeader SOAPMessage SocialSecurityValidator Socket SolidColor Sort SortError SortField Sound SoundAsset SoundChannel SoundEffect SoundEffectInstance SoundLoaderContext SoundMixer SoundTransform Spacer SpreadMethod Sprite SpriteAsset StackedSeries StackOverflowError Stage StageAlign StageQuality StageScaleMode State StateChangeEvent StaticText StatusEvent String StringUtil StringValidator Stroke StyleManager StyleSheet SwatchPanelSkin SwatchSkin SWFLoader SWFVersion SwitchSymbolFormatter SyncEvent SyntaxError System SystemManager TabBar TabNavigator TabSkin Text TextArea TextColorType TextDisplayMode TextEvent TextField TextFieldAsset TextFieldAutoSize TextFieldType TextFormat TextFormatAlign TextInput TextLineMetrics TextRange TextRenderer TextSnapshot Tile TileBase TileBaseDirection TileDirection TileListItemRenderer Timer TimerEvent TitleBackground TitleWindow ToggleButtonBar ToolTip ToolTipBorder ToolTipManager TraceTarget Transform Transition Tree TreeEvent TreeItemRenderer TreeListData TriangleItemRenderer Tween TweenEffect TweenEffectInstance TweenEvent TypeError UIComponent UIComponentCachePolicy UIComponentDescriptor UIDUtil uint UITextField UITextFormat UnresolvedConflictsError UnresolvedConflictsEvent UpdateCollectionMessage UpdateCollectionRange URIError URLLoader URLLoaderDataFormat URLRequest URLRequestHeader URLRequestMethod URLStream URLUtil URLVariables ValidationResult ValidationResultEvent Validator VBox VDividedBox VerifyError Video VideoDisplay VideoError VideoEvent ViewStack VRule VScrollBar VSlider WeakFunctionClosure WeakMethodClosure WebService WebService WedgeItemRenderer WipeDown WipeDownInstance WipeLeft WipeLeftInstance WipeRight WipeRightInstance WipeUp WipeUpInstance WSDLError XML XMLDocument XMLList XMLListCollection XMLNode XMLNodeType XMLUtil ZipCodeFormatter ZipCodeValidatorDomainType Zoom ZoomInstance	

" catch errors caused by wrong parenthesis
syn match   actionScriptInParen     contained "[{}]"
syn region  actionScriptParen       transparent start="(" end=")" contains=actionScriptParen,actionScript.*
syn match   actionScrParenError  ")"

if main_syntax == "actionscript"
  syn sync ccomment actionScriptComment
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_actionscript_syn_inits")
  if version < 508
    let did_actionscript_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink actionScriptComment		Comment
  HiLink actionScriptLineComment	Comment
  HiLink actionScriptSpecial		Special
  HiLink actionScriptStringS		String
  HiLink actionScriptStringD		String
  HiLink actionScriptCharacter		Character
  HiLink actionScriptSpecialCharacter	actionScriptSpecial
  HiLink actionScriptNumber		actionScriptValue
  HiLink actionScriptBraces		Function
  HiLink actionScriptError		Error
  HiLink actionScrParenError		actionScriptError
  HiLink actionScriptInParen		actionScriptError
  HiLink actionScriptConditional	Conditional
  HiLink actionScriptRepeat		Repeat
  HiLink actionScriptCase		Label
  HiLink actionScriptConstructor	Operator
  HiLink actionScriptObjects		Operator
  HiLink actionScriptStatement		Statement
  HiLink actionScriptFunction		Function
  HiLink actionScriptValue		Boolean
  HiLink actionScriptArray		Type
  HiLink actionScriptDate		Type
  HiLink actionScriptMath		Type
  HiLink actionScriptNumberObj		Type
  HiLink actionScriptObject		Function
  HiLink actionScriptString		Type
  HiLink actionScriptColor		Type
  HiLink actionScriptKey		Type
  HiLink actionScriptMouse		Type
  HiLink actionScriptSelection		Type
  HiLink actionScriptSound		Type
  HiLink actionScriptStage		Type
  HiLink actionScriptSystem		Type
  HiLink actionScriptTextFormat		Type
  HiLink actionScriptCommunication	Type
  HiLink actionScriptXMLSocket		Type
  HiLink actionScriptTextField		Type
  HiLink actionScriptMethods		Statement
  HiLink actionScriptException		Exception
  HiLink actionScriptXML			Type
  HiLink actionScriptArrayConstant	Constant
  HiLink actionScriptStringConstant	Constant
  HiLink actionScriptEventConstant	Constant
  HiLink actionScriptTextSnapshot	Type
  HiLink actionScriptID3			Type
  HiLink actionScriptAS2			Function
  HiLink actionScriptStyleSheet		Type
  HiLink flash8Constants		Constant
  HiLink flash8Functions		Function
  HiLink flash8Properties		Type
  HiLink flash8Classes 			Type
  HiLink actionScriptInclude		Include
  HiLink flash9Classes			Type
  HiLink flash9Constants		Function
  HiLink flash9Functions		Function
  HiLink actionscriptAS3		Function 
  delcommand HiLink
endif

let b:current_syntax = "actionscript"
if main_syntax == 'actionscript'
  unlet main_syntax
endif

" vim: ts=8

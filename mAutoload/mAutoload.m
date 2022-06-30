BeginPackage["mAutoload`", {
	 "NETLink`"
	,"CompiledFunctionTools`" (* so that one debug function is always available. forget which it is... *)
	,"mAutoload`mDocBuild`"
}];
(*ClearAll["mAutoload`*"];
ClearAll["mAutoload`Private`*"];*)


(*** troubleshooting & system options ***)
mSetInitialConfig::usage = "sets all the $FrontEnd settings you'd want on a fresh install of matheamtica";
mSetStylesheet::usage = "sets the current notebook stylesheet to specified stylesheet. by default uses the CBICalculationForm.nb stylesheet";
mToggleCompileDebug::usage = "toggles compiler debugging info";
mResetPalettes::usage = "reload palettes, clear any cached stuff";
mTestConnections::usage = "mTestConnections[] tries to open a file on all the Applications/Libraies DFS servers and shows which is currently fastest";

(*** libraries and loading ***)
$mLib::usage = "$mLib = local deployed library path (matches released programs)";
$mDev::usage = "$mDev = local deployed Libraries/Developer path (matches developer version of programs + new stuff in testing)";
$mGit::usage = "$mGit = local git working copies";
$mGite::usage = "$mGite = local git working copies";
$mGitw::usage = "$mGitw = local git working copies";
$mAppLib::usage = "$mAppLib = shared deployed Application/Libraries path";
$mAppDev::usage = "$mAppDev = shared deployed Libraries/Developer path";

mAddPath::usage = "mAddPath[dir] adds a library dir to $Path if it's not already there\n
mAddPath[dir, subDir] adds a library dir/subdir to $Path if it's not already there\n
mAddPath[dir, False] use PacletDirectoryAdd instead of adding to $Path";
mAddGitPaths::usage = "mAddGitPaths[] adds to $Path every dir in $mGit";
mInit::usage = "mInit does user's standard init.m";
mRemoveAppLib::usage = "mRemoveAppLib[] removes the Applications/Libraries path from $Path and remove the paclet";

mWhereAmI::usage = "mWhereAmI[package] returns where package's source is being read from";

(*** Copy/Paste ***)
mPasteExcel::usage = "mPasteExcel returns the contents of the Clipboard copied from Excel as List of Lists/matrix";
mPasteCode::usage = "mPasteCode prints the contents of the Clipboard with same formatting as the source, e.g. Eclipse... copy from eclipse, mPasteCode[] to paste it in mma notebook with same formatting";
mCopyCode::usage = "mCopyCode alters clipboard contents so code copied from mma pastes nicely into eclipse... copy from mathematica, mCopyCode[], paste into excel with proper formatting. may also clean weird formatting on code copied from eclipse";


(*** logging/performance/debugging ***)
mSetCacheDir::usage = "mSetCacheDir[subdir] sets location where mSaveVar/mLoadVar store caches. relative to $mmaCacheBaseDir";
mSaveVar::usage = "mSaveVar[var] saves a Symbol, var, to file. To be imported later with loadvar. Pass the Symbol, no Strings";
mLoadVar::usage = "mLoadVar[var] loads a Symbol, var, saved with savevar. Loads the same data into the same Symbol named var. Pass the Symbol, no Strings\n
mLoadVar[newVar, savedVar] loads the Symbol formerly named savedVar into a new Symbol, newVar";
mSave::usage = "mSave[var] saves a Symbol, var, and all its supporting defs? e.g. Dan's not-quite-a-class structures";
mLoad::usage = "mLoad[var] loads the data saved with mSave[var]. the receiving symbol is defined in the file and not from this function's input. May overwrite other stuff in Global`";

mVarNameValue::usage = "mVarNameValue[var] returns {varNameString, varValue}";
mVarNameValues::usage = "mVarNameValues[vars] returns {{varNameString1, varValue1}, {varNameString2, varValue2}..}";

mLog::usage = "logs stuff. works just like Print with doing more stuff. has some options to override options set in mLogStart";
mLogStart::usage = "not necessary to use. starts/resets logging stuff like the timer, target outputs, file/path. mLog uses these settings and allows overriding them at each call";
mLogGet::usage = "mLogGet[\"File\"] opens the log file. mLogGet[\"List\"] returns the list of logged data";

mDebug::usage = "mDebug[symbol] sets Global`$symbol = symbol and logs the value. Optionally Print->True prints the value of symbol.\nmDebug[] shows the debug log";
mDebugStart::usage = "mDebugSart[] clears the debug log";

mTimerStart::usage = "mStartTimer[comment, print] starts a stopwatch-y timer, log start time & comment, optionally Print it";
mTimerSplit::usage = "mSplitTimer[comment, print] splits a stopwatch-y timer, log total+split time & comment, optionally Print it";

mPerfStart::usage = "mPerfStart[] starts logging";
mPerfTime::usage = "mPerfTime[comment] logs split start with key comment. Call again with same timestamp to end";
mPerfExpr::usage = "mPerfExpr[comment]@expression logs time to execute expression with comment/label, comment";
mPerfDisplay::usage = "mPerfDisplay[] shows performance logging collected with mLog* functions";

(*** general & engineering stuff ***)
first::usage = "same as First, but if Length@thing==0 then just return thing. If thing is multidimensional return thing[[1,1,1...]] so result always has length 0";

mr2d::usage = "converts radians to degrees";
md2r::usage = "converts degrees to radians";
mm2i::usage = "meters 2 inches";
mi2m::usage = "inches 2 meters";
mm2f::usage = "meters to feet";
mf2m::usage = "feet 2 meters";
mInch2Dec::usage = "mInch2Dec creates GUI to convert fractional USC lengths to sensible base10 numbers"

(*** images/display ***)
imageRect::usage = "imageRect[image, rectangle] returns the image cropped to a rectangular ROI\n
imageRect[image, rectangle, pad] pads rectangle and returns cropped image. pad can be negative (smaller rect) or List:{{padL, padR}, {padB, padT}}";
mHue::usage = "mHue[idx, alpha] returns a Hue[] but expects an integer like an List's index. optionally provide alpha channel";

mMarkerSquare::usage	= "PlotMarkers->MarkerShape[size]";
mMarkerCircle::usage	= "PlotMarkers->MarkerShape[size]";
mMarkerDiamond::usage	= "PlotMarkers->MarkerShape[size]";
mMarkerTriangle::usage	= "PlotMarkers->MarkerShape[size]";
mMarkerPlus::usage		= "PlotMarkers->MarkerShape[size]";
mMarkerX::usage			= "PlotMarkers->MarkerShape[size]";
mMarkerAsterisk::usage	= "PlotMarkers->MarkerShape[size]";

(*** searching ***)
mFindInDir::usage = "mFindInDir[dir, targetString, filePattern] searches contents of files in dir for targetString. Returns hits";
mFindInStruct::usage = "mFindInStruct[struct, targetString] returns a list of occurrences of targetString in struct (a nesting of Association/Lists). the elements of the returned list are paste+executable I think";

(*** Session background processes? ***)
mMemSafetyStart::usage = "mMemSafetyStart[] starts a scheduled task to monitor memory usage and pops up a message to abort/quit if memory usage is too high";
mMemSafetyStop::usage = "mMemSafetyStop[] stops task started by mMemSafetyStart";

mSaveNotebooks::usage = "mSaveNotebooks saves a copy of all open, unsaved notebooks to $TemporaryDirectory/mAutoload";
mStartAutosave::usage = "mStartAutosave[interval] runs mSaveNotebooks in the background (ScheduledTask) every interval";

(*** Tests/Valdiation/Unit ***)
mTestSet::usage = "mTestSet; defines Test as mTest, so you can copy some .mt test module and run it in a notebook and have mTest run for each Test block";
mTestClear::usage = "mTestClear; clears $mTest (var used to store test results. Use before running a new set of MUnit Tests probably";
mTestReport::usage = "mTestReport[showSuccess] gives a unit test report like Wolfram Unti Tester, but in a notebook and shows all the info *&@$*~ Why doesn't unit tester show all the stuff??!?!";

(*** Info about functions/packages  ***)
mContext::usage = "mContext[symbol] gives the fully-qualified context/name of symbol";

mGetDependencies::usage = "mGetDependencies[package] reloads (Get's) a package and (Get's) all of its dependencies. Returns a list of packages reloaded.";
mGetDependencyProjects::usage = "mGetDependencyProjects[package] calls mGetDependencies and filters out duplicates and built-in packages you don't need for PacletDirectoryAdd";
mShowDependencies::usage = "mShowDependencies[package] shows ALL the dependencies for a package/context/file. Also Get's/reloads all those dependencies as a side effect...";
mGetDependencyTree::usage = "mGetDependencyTree[package] reloads (Get's) a package and (Get's) all of its dependencies. Returns a graph of package dependencies";
mShowDependencyTree::usage = "mShowDependencyTree[package] displays a graph/tree of package depencies";
$objHelp::usage = "$objHelp[object] show info about one of Dan's class/object things";

mTellMeAbout::usage = "mTellMeAbout[libraryStr] list out all the usage stuff for libraryStr, like: mTellMeAbout[\"mAutoload`\"]";
mDefinition::usage = "mDefinition[symbol] like Defintion@symbol but better. copied-ish from Spelunking";

(*** Notebook Interface ***)
mSetFocus::usage = "mSetFocus[boxId, nb] set cursor focus to control tagged with BoxID->boxId in EvaluationNotebook[] or a specified notebook if given";
mCLS::usage = "mCLS[] deletes all output cells and print statements (CLear Screen)";
mCloseGroups::usage = "mCloseGroups[] minimize/close/condense all cell groups (sections/chapters/etc)";

mEvaluateSubscripts::usage = "mEvaluateSubscripts[expr] evaluates like the styled display expression in nbCreateCheck to its intended value";

mAddQuantityInput::usage = "mAddQuantityInput[symbol, unitTypeStr] NotebookWrites a UserInterface`NotebookInterface` cell template with one quantity input that sets symbol";
mInlinePopupMenu::usage = "mInlinePopupMenu[optsList, startVal] create a PopupMenu that evaluates to what it shows (i.e. don't need Setting@)";
mInlineCheckbox::usage = "mInlineCheckbox[startVal] create a Checkbox that evaluates to what it shows (i.e. don't need Setting@)";
mInlineCompletion::usage = "mInlineCompletion[association, (initialVal)] enter the key for an association with completion helper. Evaluating cell gives value from association";
mInlineDirectoryCompletion::usage = "mInlineDirectoryCompletion[initialValue, opts] inline directory picker with browse button. evaluates as the value of the inputField";

(*** Notes to self about how funcs work ***)
$mMethod::usage = "association with keys of functions that have methods. gives a list of the methods";


Begin["`Private`"];


(*** troubleshooting & system options ***)

mSetInitialConfig := (
	SetOptions[$FrontEnd, DynamicEvaluationTimeout -> 999999];   										(* no dynamic timeout *)
	SetOptions[$FrontEnd, "NotebookSecurityOptions" -> {"TrustByDefault"->True, "UntrustedPath"->{}}];	(* don't show dynamics warnings on notebook open. doesn't help with the popup when running CDF - nothing does *)
	SetOptions[$FrontEnd, "DynamicUpdating" -> True];													(* fixes Mathematica defaulting every cell with DynamicUpdating off even though global dynamic updating is on ? Maybe bug workaround/fix *)
	SetOptions[$FrontEnd, "ExportTypesetOptions" -> {"PageWidth"->999}];								(* allow better copying, no linebreaks *)
	SetOptions[$FrontEnd, "ShowAtStartup" -> "NewDocument"];											(* no welcome screen. open direct to notebook *)
	SetOptions[$FrontEnd, "NotebookMenuHistoryLength" -> 25];											(* more quick-open files items under File  *)
	SetOptions[$FrontEnd, "CaseSensitiveCommandCompletion" -> False];									(* no welcome screen. open direct to notebook *)
	$HistoryLength = 1;
);


(*mSetDynamicTimeout := SetOptions[$FrontEnd, DynamicEvaluationTimeout->999999];   												(* no dynamic timeout *)
mSetAlwaysEnableDynamics := SetOptions[$FrontEnd, "NotebookSecurityOptions"->{"TrustByDefault"->True, "UntrustedPath"->{}}];	(* don't show dynamics warnings on notebook open. doesn't help with the popup when running CDF - nothing does *)
mSetEnableDynamics := SetOptions[$FrontEnd, "DynamicUpdating"->True];															(* fixes Mathematica defaulting every cell with DynamicUpdating off even though global dynamic updating is on ? Maybe bug workaround/fix *)
mSetCopyFormat := SetOptions[$FrontEnd,"ExportTypesetOptions"->{"PageWidth"->999}];												(* allow better copying, no linebreaks *)
mSetNotebookWelcome := SetOptions[$FrontEnd,"ShowAtStartup"->"NewDocument"];													(* no welcome screen. open direct to notebook *)*)

mSetStylesheet[sheet_:"CBICalculationForm.nb"] := SetOptions[EvaluationNotebook[], StyleDefinitions->sheet];

mToggleCompileDebug[] := With[{state=SystemOptions["CompileOptions" -> "CompileReportExternal"][[1, 2, 1, 2]]}, SetSystemOptions["CompileOptions" -> "CompileReportExternal" -> !state]; state/.{True->"On",False->"Off"}];

mResetPalettes := (
	CurrentValue[$FrontEnd, "PalettesMenuSettings"] = {};		(* clear cached settings *)
	FrontEndExecute[FrontEnd`ResetMenusPacket[{Automatic, Automatic}]];	(* reload palettes, like if added/changed palette in same session. Added second Automatic? Purpose? *)
	(*MathLink`CallFrontEnd[FrontEnd`ResetMenusPacket[{Automatic, Automatic}]]*)
);


$dfsServers = {"mcdcorp\\cbi", "DALPDFSR01", "DXBPDFSR01", "KASPDFSR01", "PLFPDFSR01"};

checkDFSConnectionsDir[timeout_:5] :=
With[{
		cnxs = Map[
			Prepend[AbsoluteTiming@TimeConstrained[DirectoryQ["\\\\" <> # <> "\\ENG\\Applications\\Libraries"], timeout], #]&
			, $dfsServers
		]
	},
	SortBy[cnxs, If[TrueQ@#[[3]], #[[2]], 999]&]
];

checkDFSConnectionsRead[timeout_:10] :=
With[{
		cnxs = Map[
			Prepend[AbsoluteTiming@TimeConstrained[ImageQ@Import["\\\\" <> # <> "\\ENG\\Applications\\Libraries\\SphereDesign\\Resources\\Images\\AISCtableA31.jpg"], timeout], #]&
			, $dfsServers
		]
	},
	SortBy[cnxs, If[TrueQ@#[[3]], #[[2]], 999]&]
];

mTestConnections[] := Grid@checkDFSConnectionsRead[];
mTestConnections[timeout_, ___] := Grid@checkDFSConnectionsRead[timeout];
mTestConnections[timeout_, "dir"] := Grid@checkDFSConnectionsDir[timeout];
mTestConnections["dir"] := Grid@checkDFSConnectionsDir[];



(*** libraries and loading ***)

$mGite = $HomeDirectory<>"\\gite";
$mGit = $HomeDirectory<>"\\git";
$mGitw = $HomeDirectory<>"\\gitw";
$mLib = $HomeDirectory<>"\\mlib";
$mDev = $HomeDirectory<>"\\mdev";
$mAppLib = "\\\\mcdcorp\\cbi\\ENG\\Applications\\Libraries";           (* "\\\\cbi\\sps\\Eng\\Applications\\Libraries" *)
$mAppDev = "\\\\mcdcorp\\cbi\\ENG\\Applications\\Libraries\\Developer"; (* "\\\\cbi\\sps\\Eng\\Applications\\Libraries\\Developer" *)
$mmaCacheBaseDir = $HomeDirectory<>"\\mma-cache";
$mmaCacheDir = $mmaCacheBaseDir;


mAddPath[dir_String /; FileExistsQ@dir] := mAddPath[dir, False];
mAddPath[dir_String, False] := If[! MemberQ[$Path, dir], PrependTo[$Path, dir]];
mAddPath[dir_String, subDir_String] := mAddPath[dir, subDir, False];
mAddPath[dir_String, subDir_String, False] := mAddPath[dir<>subDir, False];
mAddPath[dir_String /; FileExistsQ@dir, True] := PacletManager`PacletDirectoryAdd[dir];
mAddPath[dir_String, subDir_, True] := mAddPath[dir<>subDir, True];
mAddPath[x_, ___] := Message[mAddPath::nffil, x];

mAddPath::nffil = "File `1` doesn't exist or parameters in wrong format";

(* searches 2 levels down for PacletInfo.m and adds path to $Path if found (not PacletDirectoryAdd) *)
mAddGitPaths[gitPath_String:$mGitw] :=
With[{noSlashPath = If[MemberQ[{"/", "\\"}, StringTake[gitPath, -1]], StringDrop[gitPath, -1], gitPath]},
	(* always get Lib CBITools for the stylesheet. It doesn't work on the local, un-built version - I think because FrontEnd folder needs to be at same level as PacletInfo *)
	PacletManager`PacletDirectoryAdd[FileNameJoin[{$mLib,"CBITools"}]];
	mAddPath[FileNameDrop[#, -1]]& /@ FileNames["PacletInfo.m", noSlashPath, 3];
];

mInit := (
	(** User Mathematica initialization file **)
	
	$$MMALibPath="\\\\mcdcorp\\cbi\\ENG\\Applications\\Libraries";
	AppendTo[$Path,$$MMALibPath];
	(*
		Note:
		The PacletManager aggressively tries to enter every directory it finds in $$MMALibPath.
		When it does not have access to a folder (e.g. when a user doesn't have access to a certain package) it emits a General::cdir message.
		For this reason, we intentionally Quiet the General::cdir message. In newer versions General::dirdep is issued and Quieted.
		This was only done for lack of a better solution, Quiet should generally only be used only as a last resort. 
	*)
	Quiet[
		PacletManager`PacletDirectoryAdd[$$MMALibPath],
		{General::cdir, General::dirdep}
	]
);

mRemoveAppLib := (
	$$MMALibPath="\\\\mcdcorp\\cbi\\ENG\\Applications\\Libraries";
	PacletManager`PacletDirectoryRemove[$$MMALibPath];
	$Path = DeleteCases[$Path, $$MMALibPath];
);


mWhereAmI[package_:"mAutoload`"] := FileNameJoin[Most@Most@FileNameSplit@FindFile[addBacktick@package]];



(*** Copy/Paste ***)

mPasteExcel :=
Module[{clip, strData, h, w},
	InstallNET[];
	LoadNETType["System.Windows.Forms.Clipboard"];
	LoadNETType["System.Windows.Forms.TextDataFormat"];
	clip = System`Windows`Forms`Clipboard`GetText[System`Windows`Forms`TextDataFormat`UnicodeText];
	If[! StringQ@clip || clip === "",
		Message[mPasteExcel::badContent];
		Return[$Failed];
	];
	strData = StringSplit[StringSplit[clip, "\r\n"], "\t"];
	h = Length@strData;
	w = Max[Length /@ strData, 1];
	strData = PadRight[strData, {h, w}, Null];
	Map[With[{v = Quiet@ToExpression@#}, If[NumericQ@v, v, #]] &, strData, {2}]
];
mPasteExcel::badContent = "could not interpret clipboard content";


mPasteCode :=
Module[{clip},
	InstallNET[];
	LoadNETType["System.Windows.Forms.Clipboard"];
	LoadNETType["System.Windows.Forms.TextDataFormat"];
	clip = System`Windows`Forms`Clipboard`GetText[System`Windows`Forms`TextDataFormat`UnicodeText];
	NotebookWrite[EvaluationNotebook[], Cell[BoxData@StringTrim[StringReplace[clip, {RegularExpression["\r\n\t+"]->"\[IndentingNewLine]", "\r\n"->"\n"}], "\t"], "Input"]]
];

(* this probably also works to standardize code formatting *)
mCopyCode[semicolons_:False] :=
Module[{clip,depth=0,dDepth,rowTabbed,newClip},
	InstallNET[];
	LoadNETType["System.Windows.Forms.Clipboard"];
	LoadNETType["System.Windows.Forms.TextDataFormat"];
	clip = System`Windows`Forms`Clipboard`GetText[System`Windows`Forms`TextDataFormat`UnicodeText];
	
	(* strip out all indentation, we'll build it back manually *)
	If[semicolons,
		False
	];
	clip = StringReplace[clip, RegularExpression["\r?\n[ \t]*"] -> "\n"];
	
	(* TODO: doesn't handle brackets in strings. doesn't handle comments. needs to go letter by letter and suss that all out. or it's usually close enough as is *)
	newClip = Table[
		(* closing bracket de-dents on the line of closing bracket. opening bracket indents after the line with bracket. matching brackets on same line don't affect indentation. this is approximate, not actually checking for matching brackets *)
		dDepth = StringCount[row, {"[", "{"}] - StringCount[row, {"]", "}"}];
		rowTabbed = If[dDepth < 0,
			If[depth + dDepth > 0, StringRepeat["\t", depth + dDepth] <> row, row]
			,
			If[depth > 0, StringRepeat["\t", depth] <> row, row]
		];
		
		depth = Max[0, depth + dDepth];
		(*Print[depth, "/", dDepth, ": ", rowTabbed];*)
		rowTabbed
		, {row, StringSplit[clip, "\n"]}
	];
	
	CopyToClipboard@StringRiffle[newClip, "\n"];
];



(*** logging/performance/debugging ***)

mSetCacheDir[dir_String] := ($mmaCacheDir=$mmaCacheBaseDir<>"/"<>dir; Quiet@CreateDirectory[$mmaCacheDir];);
mSetCacheDir[] := ($mmaCacheDir=$mmaCacheBaseDir; Quiet@CreateDirectory[$mmaCacheDir];);

SetAttributes[mSaveVar, HoldFirst];
mSaveVar[var_Symbol] := mSaveVar[var, SymbolName@Unevaluated@var];
mSaveVar[var_Symbol, varName_String] := Export[$mmaCacheDir<>"/"<>varName<>".mx", var];

SetAttributes[mLoadVar, HoldFirst];
mLoadVar[var_Symbol] := mLoadVar[var, SymbolName@Unevaluated@var];
mLoadVar[newVar_Symbol, savedVar_String] := (newVar=Import[$mmaCacheDir<>"\\"<>savedVar<>".mx"]);

SetAttributes[mSave, HoldFirst];
mSave[var_Symbol] := mSave[var, SymbolName@Unevaluated@var];
mSave[var_Symbol, varName_String] :=
With[{fileName=$mmaCacheDir<>"\\"<>varName<>".m"},
	Save[fileName, {var}];
	fileName
];

SetAttributes[mLoad, HoldFirst];
mLoad[var_Symbol] := mLoad[var, SymbolName@Unevaluated@var];
mLoad[var_, varName_String] :=
With[{v=Get[$mmaCacheDir<>"\\"<>varName<>".m"]},
	var = v
];


SetAttributes[mVarNameValue, HoldAll];
mVarNameValue[v_] := {SymbolName@Unevaluated@v, v}

SetAttributes[mVarNameValues, HoldAll];
mVarNameValues[p___] := {ReleaseHold@Map[mVarNameValue, Hold[p]]};
mVarNameValues[{p___}] := mVarNameValues[p];



$mLogTargets = {"File", "Print", "List"};
$mLogExt = ".log";
$mLogSplit = True;
$mLogPath = Automatic;
$mLogStream = Null;
$mLogTimer = <|"t0"->ConstantArray[AbsoluteTime[], 2]|>;
$mLogList = {};

getLogPath[] := FileNameJoin[{$TemporaryDirectory, StringJoin["log-", DateString[{"Hour", "Minute", "'", "Second"}], $mLogExt]}];

Options[mLogStart] = {"Targets"->$mLogTargets, "Extension"->$mLogExt, "Time"->$mLogSplit, "Path"->$mLogPath};
mLogStart[opts:OptionsPattern[]] :=
With[{explicitOpts = Association@Flatten@List@opts, tt = ConstantArray[AbsoluteTime[], 2]},
	Quiet@Close[$mLogStream];
	$mLogStream = Null;
	$mLogTargets = Intersection[{"File", "Print", "List"}, Lookup[explicitOpts, "Targets", $mLogTargets]];
	$mLogExt = ToString@Lookup[explicitOpts, "Extension", $mLogExt];
	$mLogSplit = TrueQ@Lookup[explicitOpts, "Time", $mLogSplit];
	$mLogPath = Lookup[explicitOpts, "Path", Automatic];
	If[!StringQ@$mLogPath,
		$mLogPath = getLogPath[]
		,
		Module[{dir,file,splt},
			If[!StringContainsQ[$mLogPath, "\\"|"/"],
				splt = FileNameSplit[$mLogPath];
				dir = FileNameJoin[Most@splt];
				file = FileNameJoin[Last@splt];
				,
				dir = $TemporaryDirectory;
				file = $mLogPath;
			];
			splt  =StringSplit[file,"."];
			If[Length@splt>1 && StringMatchQ[Last@splt, RegularExpression["\\w{1,4}"]],
				$mLogExt = "."<>Last@splt;
				file = Most@splt;
			];
			$mLogPath = FileNameJoin[{dir, file<>$mLogExt}];
		];
	];
	$mLogList = {};
	$mLogTimer = <|"t0" -> tt|>;
];

Options[mLog] = Options[mLogStart];
mLog[content___, opts:OptionsPattern[]] :=
With[{
		explicitOpts = Association@Flatten@List@opts,
		t = AbsoluteTime[]
	},
With[{
		dt = {t, t} - $mLogTimer["t0"],
		targets = Lookup[explicitOpts, "Targets", $mLogTargets],
		splitQ = TrueQ@Lookup[explicitOpts, "Time", $mLogSplit]
	},
With[{
		timeComment = "+" <> ToString@mDelTime@dt[[1]] <> " | " <> ToString@mDelTime@dt[[2]] <> "\t" (*  this should be a tab but it doesn't render with PutAppend *)
	},
	$mLogTimer[["t0", 2]] = t;
	If[MemberQ[targets, "File"],
		If[!Head@$mLogStream===OutputStream,
			If[!StringQ@$mLogPath, $mLogPath = getLogPath[]];
			$mLogStream = OpenAppend[$mLogPath, PageWidth->9999, CharacterEncoding->"Unicode", FormatType->StandardForm]; (* same settings as Print = Streams[][[1]] *)
		];
		If[splitQ, print2File[timeComment, content], print2File[content]];
	];
	If[MemberQ[targets, "Print"],
		If[splitQ, Print[timeComment, content], Print[content]];
	];
	If[MemberQ[targets, "List"],
		If[splitQ, print2List[timeComment, content], print2List[content]];
	];
]]];

mLogGet[] := (Print[$mLogPath]; $mLogList);
mLogGet["List"|"list"|"l"|List] := $mLogList;
mLogGet["File"|"file"|"f"|File] := (SystemOpen[$mLogPath]; $mLogPath);

(* these are the same. Print is just Write[Streams[][[1]], v] *)
print2File[v___] := Write[$mLogStream, v];
print2List[v___] := AppendTo[$mLogList, StringJoin[ToString/@List[v]] ];



$mdebug = <||>;
mDebugStart[] := ($mdebug = <||>;);

SetAttributes[mDebug, HoldFirst];
Options[mDebug] = {"Print"->False, "$"->True};
mDebug[var_Symbol, suffix_String:"", OptionsPattern[]] :=
With[{varStr = SymbolName@Unevaluated@var},
With[{globalVar = First@StringSplit[varStr,"$"]},
	ToExpression[RowBox[{"Global`"<>If[TrueQ@OptionValue["$"],"$",""]<>globalVar<>suffix,"=", Unevaluated@var, ";"}] ];
	If[!KeyExistsQ[$mdebug, globalVar], $mdebug[globalVar]=<||> ];
	$mdebug[globalVar, DateString[{"Time", ".", "Millisecond"}]] = var;
	(*If[OptionValue["Print"], Print[RowBox[{If[TrueQ@OptionValue["$"],"$",""]<> globalVar,"=", Unevaluated@var}]] ];*)
	If[OptionValue["Print"], Print[var]];
	(*$mdebug[DateString[{"Time", ".", "Millisecond"}]] = <|varStr->var|>;*)
]];

(* this doesn't work? *)
mDebug[var_, OptionsPattern[]] :=
With[{varStr = ToString@Unevaluated@var},
With[{globalVar = First@StringSplit[Last@StringSplit[varStr,"`"],"$"]<>"$"<>First@StringReplace[StringCases[varStr,"["~~x__~~"]":>x],Except[WordCharacter]->""]},
With[{v=Evaluate@var},
(*	ToExpression[RowBox[{"Global`"<>If[TrueQ@OptionValue["$"],"$",""]<> globalVar,"=", v, ";"}] ];
	If[!KeyExistsQ[$mdebug, globalVar], $mdebug[globalVar] = <||> ];
	If[OptionValue["Print"], Print[var]];*)
	$mdebug[globalVar, DateString[{"Time", ".", "Millisecond"}]] = var;
]]];

mDebug := $mdebug;

mTime[t_] := DateString[t, {"Hour", ":", "Minute", ":", "Second", ".", "Millisecond"}];
mDelTime[dt_] := NumberForm[Round[dt, 0.001], {9, 3}];



$mTimer = <|"t0"->ConstantArray[AbsoluteTime[], 2]|>;

Options[mTimerStart] = {"Print"->True};
mTimerStart[text___, OptionsPattern[]] :=
With[{tt = ConstantArray[AbsoluteTime[], 2]},
	With[{comment = Row[{mTime@tt[[1]], "\t", text}] },
		$mTimer = <|"t0" -> tt|>;
		$mTimer[tt[[1]]] = comment;
		If[OptionValue["Print"], Print[comment] ];
	];
];

Options[mTimerSplit] = {"Print"->True};
mTimerSplit[text___, OptionsPattern[]] :=
With[{t = AbsoluteTime[]},
	With[{dt = {t, t} - $mTimer["t0"]},
		With[{comment = Row[{"+", mDelTime@dt[[1]], " | " , mDelTime@dt[[2]] , "\t", text}]},
			$mTimer[["t0", 2]] = t;
			$mTimer[t] = comment;
			If[OptionValue["Print"], Print[comment] ];
		];
	];
];


If[!ListQ@$mPerf, $mPerf = {}];

mPerfStart[] := ($mPerf = {{AbsoluteTime[], TimeUsed[], "----"}});

mPerfTime[text__:""] :=
With[{abs = AbsoluteTime[], used = TimeUsed[]},
	AppendTo[$mPerf, {abs, used, text}];
	{abs, used, text}
];


(*logExpr accepts logExpr["key"][expr]-so you can drop it in with an @   like: logExpr["pauseTime"]@Pause[1]-logs that thing "pauseTime" takes 1 second to evaluate*)
SetAttributes[mPerfExpr, HoldAll];
mPerfExpr[a__] := Function[x, mPerfExpr[a, x], HoldAll]; (*logExpr[a][x]=logExpr[Hold@a,Hold@x]*)(*?*)
mPerfExpr[text_, expr_] :=
With[{
		 nul = AppendTo[$mPerf, {AbsoluteTime[], TimeUsed[], text, 1}]
		,exprRes = Evaluate@expr
	}
	,
	AppendTo[$mPerf, {AbsoluteTime[], TimeUsed[], text, 2}];
	exprRes
];

(* don't show "end" rows. For each start, find the following end (or better: keep track of the number of times same comment is seen, start at +1, add -1 for each end, and take matching pair when level=0) *)
mPerfDisplay[] /; ($mPerf === {}) := "No Data";
mPerfDisplay[] /; And[ListQ@$mPerf , $mPerf=!={}] :=
Module[{
		entry, cacheVal, key,
		lvl = 0,
		results = {{"----", 0, "", "", ""}},
		cache = <||>,
		t0 = $mPerf[[1, 1]]
	}
	,
	Do[
		key = entry[[3]];
		If[Length@entry==4,
			If[entry[[4]] == 1,
				lvl += 1;
				cache[{key, lvl}] = {Length@results + 1, entry[[1]], entry[[2]]};
				AppendTo[results, {Row@Append[ConstantArray[".", Max[lvl-1,0]], key], mDelTime[entry[[1]] - t0], "", "", ""}]
				,
				cacheVal = cache[{key, lvl}];
				(*results[[First@cacheVal, 3]] = mDelTime[entry[[1]] - t0];
				results[[First@cacheVal, 4]] = mDelTime[entry[[1]] - cacheVal[[2]]];
				results[[First@cacheVal, 5]] = mDelTime[entry[[2]] - cacheVal[[3]]];*)
				results[[First@cacheVal, 3;;5]] = mDelTime/@{entry[[1]]-t0, entry[[1]]-cacheVal[[2]], entry[[2]]-cacheVal[[3]]};
				lvl -= 1;
			];
			,
			If[key=!="----",
				AppendTo[results, {Row[{"-", key}], mDelTime[entry[[1]] - t0], "", "", ""}]
			]
		]
		, {entry, Rest@$mPerf}
	];

	(* total time logged under the first entry *)
	results[[1, 3;;5]] = mDelTime/@{$mPerf[[-1, 1]]-t0, $mPerf[[-1, 1]]-$mPerf[[1, 1]], $mPerf[[-1, 2]]-$mPerf[[1, 2]]};

	Grid[
		Join[
			{Style[#, Bold] & /@ {"comment", "start", "end", "abs\[CapitalDelta]", "cpu\[CapitalDelta]"}}
			,results
		]
		, Spacings->{2, .5}, BaseStyle->"InlineFormula", Alignment->{{Left,Right,Right,Right,Right},Baseline}
	]
];



(*** general & engineering stuff ***)

addBacktick[context_] := If[StringTake[context, -1] != "`", context<>"`", context];

safeDiv[0|0, _] := 0;
safeDiv[num_, 0|0.] := Sign[num]*Infinity;
safeDiv[num_, denom_] := num/denom;

first[v_ /; Length@v != 0] := First@v;
first[v_] := v;

mr2d[v_] := 180*v/Pi;
md2r[v_] := Pi*v/180;
mm2i[v_] := v*3.28083989501*12;
mi2m[v_] := v*0.3048/12;
mm2f[v_] := v*3.28083989501;
mf2m[v_] := v*0.3048;


(* this one needs some refinement - GUI to convert to/from decimal & foot' inch" *)
mInch2Dec := DynamicModule[{ft = 0, in = 0, fr = 0, sxth = 0, cpyInchs, nb, denom = 16, denomOpts},
	cpyInchs[feet_, inch_, numer_] := CopyToClipboard[N[12*feet + inch + numer/16]];
	
	nb = EvaluationNotebook[];
	denomOpts = Map[# -> Superscript[ToString@#, "ths"] &, {8, 16, 32, 64}];
	
	Grid[
		 {Style[#, 10] & /@ {"Feet", "Inch", PopupMenu[Dynamic@denom, denomOpts], "Frac", "Decimal"}
		,{
			 InputField[
				Dynamic[ft
					, (ft = #; cpyInchs[ft, in, sxth]; If[ft > 4, mSetFocus["IN", nb]])&
				]
				, Number, ContinuousAction -> True, FieldSize -> 2, BoxID -> "FT"
			]
			,InputField[
				Dynamic[in
					, (in = #; cpyInchs[ft, in, sxth]; If[in > 1, mSetFocus["SX", nb]]) &
				]
				, Number, ContinuousAction -> True, FieldSize -> 2, BoxID -> "IN"
			]
			,InputField[
				Dynamic[sxth
					, (sxth = #; fr = InputForm[Round[sxth/denom, 1/denom]]; cpyInchs[ft, in, sxth]; If[sxth > 1, mSetFocus["FT", nb]]) &
				]
				, Number, ContinuousAction -> True, FieldSize -> 2, BoxID -> "SX"
			]
			,InputField[
				Dynamic[fr
					, (sxth = IntegerPart@Round[#*denom]; cpyInchs[ft, in, sxth]; fr = InputForm@#; mSetFocus["FT", nb]) &
				]
				, Expression, ContinuousAction -> False, FieldSize -> 3.5, BoxID -> "FR"
			]
			,InputField[
				Dynamic[12.*ft + in + sxth/denom
					, (ft = IntegerPart[#/12]; in = IntegerPart@Mod[#, 12]; sxth = IntegerPart@Mod[denom*#, denom]) &
			], FieldSize -> 5]
		}
	}]
];


(*** images/display ***)

(* I think this is the best way to extract a rectanglular ROI from an image? what a mess *)
imageRect[img_Image, rect_] := With[{size = ImageDimensions@img}, ImageTake[img, size[[2]] - {rect[[2, 2]], rect[[1, 2]]}, {rect[[1, 1]], rect[[2, 1]]}]];
imageRect[img_Image, rect_, pad_Integer] := imageRect[img, rect, {{pad, pad}, {pad, pad}}];
imageRect[img_Image, rect_, pad : {{padL_, padR_}, {padB_, padT_}}] := With[{size = ImageDimensions@img}, ImagePad[ImageTake[img, size[[2]] - {rect[[2, 2]], rect[[1, 2]]}, {rect[[1, 1]], rect[[2, 1]]}], pad]];
imageRect[___] := $Failed;


(* just a color funciton. Gives 9 vibrant colors cycled. Optional opacity/alpha *)
mHue[rNo_, alpha_:1] := Hue[Mod[rNo, 10]/10, 1, 1, alpha];


(* to use: PlotMarkers -> mSquareMarker[faceColor, edgeColor, markerSize] *)
marker[graphicsPrimative_, opts___][size_:13, innerColor_, outerColor_] :=
	Graphics[{innerColor, EdgeForm[{AbsoluteThickness[2], Opacity[1], outerColor}], graphicsPrimative}, opts, ImageSize->size];

marker[graphicsPrimative_, opts___][size_:13, color_:Nothing] :=
	Graphics[{color, graphicsPrimative}, opts, ImageSize->size];

mMarkerSquare	= marker@Rectangle[];
mMarkerCircle	= marker@Disk[];
mMarkerDiamond	= marker@Polygon[{{0, 1}, {1, 2}, {2, 1}, {1, 0}}];
mMarkerTriangle	= marker[Polygon[{{1, 0}, {0, Sqrt[3]}, {-1, 0}}], AlignmentPoint -> {0, 1/Sqrt[3]} ];
mMarkerPlus		= marker@{Thickness[.2], Line[{{0, 1}, {2, 1}} ], Line[{{1, 0}, {1, 2}}] };
mMarkerX		= marker@{Thickness[.2], Line[{{0, 0}, {2, 2}} ], Line[{{0, 2}, {2, 0}}] };
mMarkerAsterisk	= marker@{Thickness[.2], Line[{{0, 1}, {2, 1}} ], Line[{{1, 0}, {1, 2}}], Line[{{0, 0}, {2, 2}}], Line[{{0, 2}, {2, 0}}] };



(*** searching ***)

Options[mFindInDir] = {"IgnoreCase"->True, "Depth"->Infinity, "SecondPattern"->Null, "Print"->True};
mFindInDir[startDir_String, targetString_String, filePattern_String: "*", OptionsPattern[]] :=
Module[{files, matchedLines, fileName, results, subFile, p2, doPrint, doCase},
	p2 = OptionValue["SecondPattern"];
	doPrint = TrueQ@OptionValue["Print"];
	doCase = TrueQ@OptionValue["IgnoreCase"];
	
	files = FileNames[filePattern, startDir, OptionValue["Depth"]];
	results = {};
	Do[
		matchedLines = FindList[fileName, targetString, IgnoreCase -> doCase];
		If[matchedLines =!= {},
			subFile = Last@StringSplit[fileName, startDir];
			Map[
				If[p2 =!= Null && StringContainsQ[#, p2, IgnoreCase -> doCase],
					AppendTo[results, {subFile, #}];
					If[doPrint, Print[subFile, " - ", #]];
				]&
				, matchedLines
			]
		]
		, {fileName,  files}
	];
	
	results
];


(* I dunno if this actually works here... *)
SetAttributes[mFindInStruct, HoldFirst];
mFindInStruct[struct_, target_String] := mFindInStruct[struct, target, SymbolName@Unevaluated@struct];

mFindInStruct[assc_Association, target_String, path_String] :=
Module[{
		result = Map[StringJoin[path, wrapKey@#] &, Cases[ToString /@ Keys@assc, _?(StringContainsQ[ToString@#, target, IgnoreCase->True] &)]]
	},
	result = Join[result, Map[mFindInStruct[assc[#], target, path <> wrapKey@#] &, Keys@assc]];
	If[result === {}, Nothing, result]
];

mFindInStruct[list_List, target_String, path_String] :=
Module[{result},
	result = Map[mFindInStruct[list[[#]], target, path<>wrapPart@#] &, Range@Length@list];
	If[result === {}, Nothing, result]
];

mFindInStruct[rule_Rule, target_String, path_String] :=
Module[{key = ToString@Keys@rule, result},
	result = If[StringContainsQ[key, target, IgnoreCase->True], path<>wrapKey@key, {}];
	Join[result, mFindInStruct[Values@rule, target, path<>wrapKey@key]];
	If[result === {}, Nothing, result]
];

mFindInStruct[val_String, target_String, path_String] :=
With[{strVal = ToString@val},
	If[StringContainsQ[strVal, target, IgnoreCase->True], path <> "=" <> strVal, Nothing]
];

mFindInStruct[___] := Nothing;


wrapKey[key_] := "["<>ToString@key<>"]";
wrapPart[part_] := "[["<>ToString@part<>"]]";



(*** Session background processes? ***)

$maxMemAllowed = 8.0*1024^3;
$intervalBetweenTests = 1;
$memSafetyTask = Null;

mMemSafetyStart[] := (
	mMemSafetyStop[];
	$memSafetyTask = RunScheduledTask[
		If[MemoryInUse[] > $maxMemAllowed,
			mMemSafetyStop[];
			Switch[mMemFailDialog[]
				,1, KernelExecute[Quit[]];
				(*,2, FrontEndExecute[FrontEnd`EvaluatorInterrupt]*)
				,2, FrontEndTokenExecute["EvaluatorAbort"];
				,3, $maxMemAllowed *= 2; mMemSafetyStart[]
			]
		]
		, $intervalBetweenTests
	];
);

mMemSafetyStop[] :=
If[$memSafetyTask =!= Null,
	RemoveScheduledTask[$memSafetyTask]; $memSafetyTask = Null;
];

mMemFailDialog[] := (
	CreateDialog[
		Column[{
			 "\[WarningSign] Memory Use Exceeds Limit"
			,Button["QUIT",   DialogReturn[1], Background -> Lighter@Red]
			,Button["ABORT", DialogReturn[2], Background -> Lighter@Yellow]
			,Button["IGNORE", DialogReturn[3], Background -> Lighter@Green]
		}]
		, WindowFrame -> "ModalDialog", WindowFrameElements->None, WindowTitle -> "High RAM"
	]
);


mSaveNotebooks :=
Module[{saveDir, nbs, info, saveFile},
	saveDir = FileNameJoin[{$TemporaryDirectory, "mAutosave"}];
	nbs = Notebooks[];
	
	Quiet@CreateDirectory@saveDir; (*Create ".../AppData/Local/Temp/mAutosave/ if not exists *)
	
	(* save a copy of each notebook. If you just do NotebookSave[nb,location] then it changes the name/location of the open notebook.NotebookPut@NotebookGet creates a copy *)
	Do[
		info = Association@NotebookInformation[nb];
		(* only save notebooks that have changed & skip the Messages window that's always lurking *)
		If[info["ModifiedInMemory"] && info["DocumentType"] == "Notebook" && info["WindowTitle"] =!= "Messages",
			saveFile = If[KeyExistsQ[info, "FileName"],
				(* notebook file has been saved before. Save as bottomLevelFolder~FileName & hope it's unique-ish *)
				FileNameJoin[{saveDir, info["FileName"][[1, -1]] <> "~" <> info["FileName"][[2]]}]
				,
				(* notebook not saved before. Save as UUID~WindowTitle so it's unique because it's probably called Untitled-1 *)
				FileNameJoin[{saveDir, info["ExpressionUUID"] <> "~" <> info["WindowTitle"]}]
			];
			Export[saveFile, NotebookGet@nb];
		]
		, {nb, nbs}
	];
];

(* this runs asynchronously in current session. It's not possible to use LocalSubmit and run in separate kernel because it won't see Notebooks[] *)
mStartAutosave[timeout_:180] := (
	If[Head@$mSaveTask === TaskObject, Quiet@TaskRemove[$mSaveTask]];
	$mSaveTask = SessionSubmit[
		ScheduledTask[
			mSaveNotebooks
			, timeout
		]
	];
);


(* wrap and compress function definition for evaluating with another kernel, like LocalSubmit *)
SetAttributes[compressWithDefinitions, HoldFirst];
compressWithDefinitions[symb_] := With[{def=Language`ExtendedFullDefinition[symb]}, Compress@Unevaluated[Language`ExtendedFullDefinition[] = def; symb]]

(* example? *)
(*Module[{res},
	With[{s = compressWithDefinitions@DownValues[func]},
		LocalSubmit[
			Uncompress[s]
			, HandlerFunctions -> <|"TaskFinished" -> ((res = #EvaluationResult) &)|>
			, HandlerFunctionsKeys -> "EvaluationResult"
		]
	];
	res
];*)





(*** Tests/Valdiation/Unit ***)

$mCheckDebug = True;

mCheck[a_, b_] := mCheck[a, b, $$AbsTol, $$RelTol];

mCheck[a_Association, b_Association, relTol_?NumericQ, absTol_?NumericQ] :=
If[Sort@Keys@a =!= Sort@Keys@b,
	If[$mCheckDebug, Print["Association keys differ:\n", a, " /\n", b] ];
	False
	,
	And @@ Map[mCheck[a[#], b[#], relTol, absTol] &, Keys@a]
];

mCheck[a_List, b_List, relTol_?NumericQ, absTol_?NumericQ] :=
If[Length@a != Length@b,
	If[$mCheckDebug, Print["List lengths don't match:\n", a, " /\n", b] ];
	False
	,
	And @@ MapThread[mCheck[#1, #2, relTol, absTol] &, {a, b}]
];

mCheck[a_Quantity, b_Quantity, relTol_?NumericQ, absTol_?NumericQ] := mCheck[QuantityMagnitude@UnitConvert@a, QuantityMagnitude@UnitConvert@b, relTol, absTol];

mCheck[a_?NumericQ, b_?NumericQ, relTol_?NumericQ, absTol_?NumericQ] :=
Which[
	 relativeErrorCheck[a, b, relTol], True
	,absoluteErrorCheck[a, b, absTol], True
	,True, If[$mCheckDebug, Print["Check failure: ", a, " / ", b] ]; False
];

mCheck[a_, b_, relTol_?NumericQ, absTol_?NumericQ] :=
(
	If[$mCheckDebug, Print["Unrecognized types or type mismatch: ", Head@a, " / ", Head@b] ];
	False
);


$mTest = {};
$mTestNo = 0;
$mErrParams = {};

SetAttributes[sowHold, HoldAll];
sowHold[p___] := Sow@HoldForm@p;

SetAttributes[splitP, HoldAll];
splitP[f_[p___]] := With[{pv = p}, HoldForm[f[pv]]];
(*splitP[f_[p___]] := HoldForm[f[Evaluate@p]];*)


(*
(* another way to capture messages, probably better *)
ClearAll[messageHandler];
messageHandler[p__] := AppendTo[$msgs, tag[p]];
Internal`AddHandler["Message", messageHandler];
...
Internal`RemoveHandler["Message", messageHandler];

*)

(* The Test function gets redirected to here *)
Options[mTest] = {SameTest->Equal, TestID->Null};
SetAttributes[mTest, HoldAll];
mTest[actual_, expected_, msgs_List, opts:OptionsPattern[]] :=
Block[{$MessagePrePrint=AppendTo[$mErrParams, #]&},
	(*actParams = Reverse@First@Last@Reap[Scan[sowHold, Hold[actual], Infinity]]; (* this displays the whole evaluation tree if actual function/value is inside the Test, kinda neat, but too complicated to polish up *)
	actCall = Grid[{actParams, ReleaseHold/@actParams}];*)
	Quiet@AppendTo[$mTest, {
		 ++$mTestNo
		,OptionValue[TestID]
		,actual
		,expected
		,Quiet@OptionValue[SameTest][actual, expected] && Complement[$MessageList,Thread[HoldForm[msgs]]]==={}
		,$MessageList
		,$mErrParams
		,Thread[HoldForm[msgs]] (* keep the list of messages in {Function::error..} form *)
		,HoldForm@actual (* if function is inside the test, this gives the top-level of the evaluation. If it has parameters like x$12345, you can (manually) evaluate x$12345 in Global` scope and see value *)
		(*,splitP@actual*)
	}];
	$mErrParams = {};
	$MessageList={};
];
mTest[actual_, expected_, opts:OptionsPattern[]] :=
Block[{$MessagePrePrint=AppendTo[$mErrParams, #]&},
	(*actParams = Reverse@First@Last@Reap[Scan[sowHold, Hold[actual], Infinity]]; (* this displays the whole evaluation tree if actual function/value is inside the Test, kinda neat, but too complicated to polish up *)
	actCall = Grid[{actParams, ReleaseHold/@actParams}];*)
	Quiet@AppendTo[$mTest, {
		++$mTestNo
		,OptionValue[TestID]
		,actual
		,expected
		,Quiet@OptionValue[SameTest][actual, expected] && $MessageList=={}
		,$MessageList
		,$mErrParams
		,{}
		,HoldForm@actual (* if function is inside the test, this gives the top-level of the evaluation. If it has parameters like x$12345, you can (manually) evaluate x$12345 in Global` scope and see value *)
		(*,splitP@actual*)
	}];
	$mErrParams = {};
	$MessageList={};
];

mBeginTestSection[sectionName_] :=
Block[{$MessagePrePrint=AppendTo[$mErrParams, #]&},
	(*actParams = Reverse@First@Last@Reap[Scan[sowHold, Hold[actual], Infinity]]; (* this displays the whole evaluation tree if actual function/value is inside the Test, kinda neat, but too complicated to polish up *)
	actCall = Grid[{actParams, ReleaseHold/@actParams}];*)
	AppendTo[$mTest, sectionName];
	$mErrParams = {};
];


mTestClear := ($mTest={}; $mTestNo=0;);
mTestSet := (
	ToExpression["Global`Test=mAutoload`Private`mTest"];
	ToExpression["Global`BeginTestSection=mAutoload`Private`mBeginTestSection"];
	Unprotect@$MessageList;
	Off[General::stop];
	(*ToExpression["Global`EndSection=mAutoload`Private`mEndSection"];*)
); (* this garbage so no errors on kernel start... *)

mTestReport[_] /; SameQ[$mTest, {}] := "No test results stored!";
mTestReport[showSuccess_:False] :=
With[{precs = CurrentValue[EvaluationNotebook[], {PrintPrecision}]},
	SetOptions[EvaluationNotebook[], {PrintPrecision->12}];
	With[{
		out = If[!showSuccess && AllTrue[$mTest, StringQ@# || TrueQ@#[[5]] &],
			Highlighted["All Tests Pass", Background->Green, BaseStyle->{Larger, Bold, FontFamily->"Roboto"}]
			,
			Column[ Map[mTestDisplayResult[showSuccess, #]&, $mTest] //. {x___, __Row, b_Row, y_OpenerView, z___} :> {x, b, y, z} ]
		]
	},
	SetOptions[EvaluationNotebook[], {PrintPrecision->precs}];
	out
]];

mTestDisplayResult[showSuccess_, sectionName_] := Row[{sectionName}, BaseStyle->"Subsection"];
mTestDisplayResult[showSuccess:False, {testNo_, testID_, actual_, expected_, result_?TrueQ , msgList_, msgParams_, expectedMsgs_, rawActual_}] := Nothing;
mTestDisplayResult[showSuccess_, {testNo_, testID_, actual_, expected_, result_, msgList_, msgParams_, expectedMsgs_, rawActual_}] :=
OpenerView[{
	 Button[Highlighted[Row[{testNo, ". ", testID}], Background -> If[result == True, Green, Red, Yellow]], CopyToClipboard[testID], Appearance->"Frameless", BaseStyle->{"Input", Larger}]
	,Column[{
		 Row[{"Call: ", rawActual}]
		,Row[{"Actual: ", actual}]
		,Row[{"Expect: ", expected}]
		,With[{diff=actual-expected},
			If[NumericQ@diff||QuantityQ@diff, Row[{"\[CapitalDelta]:", diff, "  ", Subscript["\[CapitalDelta]:","%"], 100.0*safeDiv[diff, expected] }], Nothing]
		]
		,If[TrueQ@result, Nothing, Row[{"Comparison: ", result}] ]
		,If[Length@msgList < 1 && Length@expectedMsgs < 1, Nothing
			,Column[{
				 Style["Messages", Underlined]
				,If[Length@msgList==0, "None",
					Sequence@@Table[
						Row[{msgList[[n]], " [", If[Length@msgParams > n, msgParams[[n]], " "], "]"}]
						, {n, Length@msgList}
					]
				]
				,Style["Expected Messages", Underlined]
				,If[Length@expectedMsgs==0, "None",
					Sequence@@Table[
						Row[{expectedMsgs[[n]]}]
						, {n, Length@expectedMsgs}
					]
				]
			}]
		]
	}]
},False
];



(*** Info about functions/packages  ***)

(* https://mathematica.stackexchange.com/questions/17916/for-any-symbol-how-can-i-get-the-full-context-qualified-name-of-the-symbol-as-a *)
(* longer version returns:  primary symbol name if defined ___ any private definitions --- and symbols (partially) containing the sym *)
SetAttributes[mContext, {Listable, HoldAll}];
mContext[sym_] :=
Module[{names, completions, fullName, symbStr = ToString@Unevaluated@sym},
	Block[{Internal`$ContextMarks = True},
		names = Names["*`" <> symbStr];
		completions = Names["*`" <> symbStr <> "*"];
		fullName = ToString@Unevaluated@sym;
		Column[
			If[StringContainsQ[#, "`"], Button[#, CopyToClipboard[#], Appearance->"Frameless", BaseStyle->"Output"], #]& /@
			Select[DeleteDuplicates[Join[If[ValueQ@sym || ! StringStartsQ[fullName, "Global`"], {fullName}, {}], {"_________"}, names, {"---------"}, completions]], StringContainsQ[#, {"`", "-", "_"}] &]
		]
	]
];

(* force reload all of the package (usually...) and return a list of ALL the packages that were loaded in the process (that origContext requires to run) *)
(* Can provide a top-level context, e.g. "UmbrellaRoofDesign`" or a sub-context like "UmbrellaRoofDesign`NotebookInterface". backtick not needed *)
(* it'd be swell if it didn't actually reread every package... *)
mGetDependencies[origContext_String] :=
With[{ignoredPackages = {"PacletManager`", "URLUtilities`", "DatabaseLink`"} },
Module[{$mPackages = ignoredPackages},
	Internal`InheritedBlock[{Needs, $Packages},
		Unprotect[Needs, $Packages];
		
		Needs[cntxt_String] /; !MemberQ[$mPackages, cntxt] :=
			Module[{},
				AppendTo[$mPackages, cntxt];
				$Packages = DeleteCases[$Packages, cntxt]; (* trick mathematica into re-reading the package even if it's already loaded *)
				Quiet@With[{clearContext = StringJoin[cntxt, "*"]}, ClearAll[clearContext]];
				Needs[cntxt];
			];
		
		Quiet@Needs[addBacktick@origContext];
		
(*		(*reset Needs to orignal def *)
		Clear[Needs];
		Protect[Needs, $Packages];*)
		
		Complement[$mPackages, ignoredPackages]
	]
]];

(* also reloads the package... *)
mShowDependencies[origContext_String] := mShowDependencies[mGetDependencies[origContext]];
mShowDependencies[contextList_List] := Grid[Partition[Sort[contextList], 3], Alignment->Left, Spacings->2];


mGetDependencyProjects[origContext_String] :=
Complement[
	Union@Map[
		First@StringSplit[#, "`"]&
		, Append[mGetDependencies[origContext], "CBITools`"] (* always need CBITools for stylesheet *)
	]
	, {"NETLink", "JLink", "LightweightGridClient", "Parallel", "ResourceLocator", "SubKernels", "GeneralUtilities", "Macros", "TypeSystem"} (* built-in packages. Don't require explicit PacletDirectoryAdd's *)
];


(* just for fun *)
(* this is flawed. edges in graph only show the first time a new file is included, not every pakage that gets that file *)
mGetDependencyTree[origContext_String] :=
Module[{$mPackages = <|"PacletManager`" -> True, "URLUtilities`" -> True, "DatabaseLink`" -> True, "JLink`" -> True|>, gEdges = {}},
	$parent = ".";
	Unprotect[Needs, $Packages];
	Needs[cntxt_String] /; ! KeyExistsQ[$mPackages, cntxt] := (
		AppendTo[gEdges, Column@StringSplit[$parent, "`"] -> Column@StringSplit[cntxt, "`"]];
		$mPackages[cntxt] = True;
		$Packages = DeleteCases[$Packages, cntxt];
		Quiet@With[{clearContext = StringJoin[cntxt, "*"]}, ClearAll[clearContext]];
		Block[{$parent = cntxt}, Needs[cntxt] ];
	);
	
	Quiet@Needs[addBacktick@origContext];
	Protect[Needs, $Packages];
	Unprotect[Needs]; Clear[Needs]; Protect[Needs];(*reset Needs to orignal state*)
	<|
		 "packages" -> DeleteCases[Keys@$mPackages, _?(MemberQ[{"PacletManager`", "URLUtilities`"}, #] &)]
		, "edges" -> Drop[gEdges, 1]
	|>
];

mShowDependencies[origContext_String] := mShowDependencyTree[mGetDependencyTree[origContext]];
mShowDependencyTree[data_Association] :=
Module[{vertices,panelLabel,vertexLabels},
	vertices = Union[Flatten[List @@@ data["edges"]]];
	panelLabel[lbl_] := Panel[lbl, FrameMargins -> 0, BaseStyle -> Small];
	vertexLabels = Map[# -> Placed[#, Center, panelLabel] &, vertices];
	Graph[Flatten@data["edges"], VertexLabels -> vertexLabels, ImageSize -> 1600, VertexSize -> .05, EdgeShapeFunction -> GraphElementData["ShortUnfilledArrow", "ArrowSize" -> 0.01]]
];


parseStupidFunction[stupidFunction_] :=
With[{pattern = StringCases[ToString@stupidFunction, {
		 RegularExpression["`([^`]*)\$.*\[(.*)"] -> "$1[$2"		(* UmbrellaRoofDesign`UmbrellaRoofDesignStandards`Private`setDesignStandardImpl$9628[##1] & -> setDesignStandardImpl[##1]& *)
		,RegularExpression["`([^`]*)\$"] -> "$1"				(* UmbrellaRoofDesign`UmbrellaRoofDesignStandards`Private`setDesignStandardImpl$9628NJS*($?? -> setDesignStandardImpl *)
	}]
	},
	If[pattern === {}, stupidFunction, StringReplace[First@pattern, " " -> ""]]
];

$objHelp[obj_Association] :=
Grid[{
		 Style[First@#, Bold]
		,parseStupidFunction[Last@# /. {f_Function[a_, b_] :> a}] (* TODO: not Function here I think but it hits List on {a, b} so that's no good *)
	}& /@ Normal@obj
	, Alignment -> {{Right, Left}}
];
$objHelp[obj_] :=
Grid[{
		 Style[First@#, Bold]
		,parseStupidFunction[Last@# /. {f_Function[a_, b_] :> a}]
	}& /@ Normal@First@obj
	, Alignment -> {{Right, Print[Head@First@obj];Left}}
];


(* show usage info for public functions package/file. TODO: mTellMeAbout[Package`] should map over all subpackages and show parameters like fancy definition instead of relying on usage *)
mTellMeAbout[libraryStr_String /; StringTake[libraryStr,-1]!="`"] := mTellMeAbout[libraryStr<>"`"];
mTellMeAbout[libraryStr_String /; StringTake[libraryStr,-1]=="`"] :=
With[{names = Names[libraryStr<>"*"]},
If[names === {},
	Column[
		Button[#,
			NotebookWrite[EvaluationNotebook[],
				Cell[BoxData@ToBoxes@Column[{#, mTellMeAbout[#]}], "Output"]
			]
		] & /@ Select[$ContextPath, StringStartsQ[#, libraryStr] &]
		, Alignment -> Left
	]
	,
	Grid[
		Table[
			Module[{
					 shortFuncName = Last@StringSplit[funcName, "`"]
					,usageText = ToExpression[funcName<>"::usage"]
					,cases, params, descrip
				},
				(* usageText is like: functionName[params] = descrip   extract params and descrip *)
				If[MatchQ[usageText, _String],
					cases = StringCases[usageText, RegularExpression["\\w+\[([^\]]+)\\W*(.*)"] -> {"$1", "$2"}];
					(* todo: handle function[version][params] *)
					If[Length@cases > 0,
						params = cases[[1, 1]];
						descrip = cases[[1, 2]];
						,
						params = usageText;
						descrip = SpanFromLeft;
					];
					,
					params = "";
					descrip = "";
				];
				
				{Style[shortFuncName, Bold],  params, descrip}
			]
			, {funcName, names}
		], Alignment->Left
	]
]];

(* copied from CBIDeveloperTools`Spelunking  shows function definition. prints Cells. Combine with mTellMeAbout sometime *)
mDefinition[symbol_Symbol] :=
NotebookWrite[EvaluationNotebook[],
	Cell[
		BoxData[
			prettyBoxes[
				defBoxes[symbol] /.
					s_String?(StringMatchQ[#, __ ~~ "`" ~~ __] &) :> First@StringCases[s, a:(__ ~~ "`" ~~ b__) :> processSymbol[a, b] ]
			]
		]
		, "Output"
		, ShowStringCharacters -> True
		, Background -> RGBColor[1, 0.95, 0.9]
		, CellGroupingRules->"OutputGrouping"
		, GeneratedCell->True
		, CellAutoOverwrite->True
		, ShowAutoStyles->True
		, LanguageCategory->"Mathematica"
		, FontWeight->"Bold"
	]
];

defBoxes[symbol_Symbol] :=
Hold[symbol] /. _[sym_] :>
	If[MemberQ[Attributes[sym], Locked],
		"Locked"
		,
		Internal`InheritedBlock[{sym},
			Unprotect[sym];
			ClearAttributes[sym, ReadProtected];
			Quiet@Check[ToBoxes[Definition@sym], "DefError"] /. InterpretationBox[a_, b___] :> a
		]
	];

defBoxes[s_String] := defBoxes@ToExpression[s, InputForm, Unevaluated]

prettyBoxes[boxes_] :=
	boxes /. {" "} -> {"\n-----------\n"} //. {
	(*boxes /. {" "} -> Nothing //. {*)
		 RowBox[{left___, ";",  next:Except["\n"], right___}] :> RowBox[{left, ";", "\n", "\t", next, right}]
		,RowBox[{sc:("Block" | "Module" | "With"), "[", RowBox[{vars_, ",", body_}], "]"}] :> RowBox[{"\n", sc, "[", RowBox[{vars, ",", "\n\t", body}], "]"}]
	};


processSymbol[a_, b_] :=
Module[{db},
	Which[
		 !StringFreeQ[a, "\""],	a
		,!StringFreeQ[a, "_"] || (db = defBoxes[a]) === "Null",	TooltipBox[b, a]
		,db === "Locked", 	TooltipBox[b, a <> "\nLocked Symbol"]
		,db === "DefError",	TooltipBox[b, a <> "\nError getting Definition"]
		,True,
			ActionMenuBox[
				TooltipBox[StyleBox[b, FontVariations->{"Underline"->True}], a]
				, {
					"Discover function" :> mDefinition[a],
					"Copy full name" :> CopyToClipboard@Cell[a, "Input"]
				}
				, DefaultBaseStyle -> {"Input"}, Appearance->"None", Evaluator -> Automatic
			]
	]
];



(*** Notebook Interface ***)

mSetFocus[boxID_String, nb_:Null] := (FinishDynamic[]; MathLink`CallFrontEnd[FrontEnd`BoxReferenceFind[FE`BoxReference[If[nb===Null,EvaluationNotebook[], nb], {{boxID}}, FE`BoxOffset -> {FE`BoxChild[1]}, FE`SearchStart -> "StartFromBeginning"]]]);


mCLS := NotebookDelete[Cells[EvaluationNotebook[], CellStyle -> {"Message", "Print", "Output"}]];

mCloseGroups := (
	FrontEndExecute[FrontEndToken[EvaluationNotebook[], "SelectAll"]];
	FrontEndTokenExecute["SelectionCloseAllGroups"];
);


(* helper to create replicate cell template, or just type in values *)
$unitSys = "Metric"; (* set somewhere? palette? *)
dfltUnit[unitType_] := dfltUnitImpl[unitType, $unitSys];
dfltUnitImpl["Length", "USC"] = "Inches";
dfltUnitImpl["Length", "Metric"] = "Millimeters";
dfltUnitImpl["Area", "USC"] = "Inches"^2;
dfltUnitImpl["Area", "Metric"] = "Millimeters"^2;
dfltUnitImpl["Stress", "USC"] = "PoundsForce"/"Inches"^2;
dfltUnitImpl["Stress", "Metric"] = "Megapascals";
dfltUnitImpl["Force", "USC"] = "PoundsForce";
dfltUnitImpl["Force", "Metric"] = "Kilonewtons";
dfltUnitImpl["Moment", "USC"] = "PoundsForce"*"Inches";
dfltUnitImpl["Moment", "Metric"] = "Kilonewtons"*"Meters";
dfltUnitImpl["Angle", "USC"] = "AngularDegrees";
dfltUnitImpl["Angle", "Metric"] = "AngularDegrees";
dfltUnitImpl["Density", "USC"] = "Pounds"/"Inches"^3;
dfltUnitImpl["Density", "Metric"] = "Kilograms"/"Meters"^3;


SetAttributes[mEvaluateSubscripts, HoldAll];
mEvaluateSubscripts[Plus[a_, b_]] := mEvaluateSubscripts[a] + mEvaluateSubscripts[b];
mEvaluateSubscripts[Times[a_, b_]] := mEvaluateSubscripts[a]*mEvaluateSubscripts[b];
mEvaluateSubscripts[Power[a_, b_]] := mEvaluateSubscripts[a]^mEvaluateSubscripts[b];
mEvaluateSubscripts[Rational[a_, b_]] := mEvaluateSubscripts[a]/mEvaluateSubscripts[b];
mEvaluateSubscripts[Sqrt[a_]] := Sqrt[mEvaluateSubscripts[a]];
mEvaluateSubscripts[Subscript[a_, b_]] := mEvaluateSubscripts[a, b];
mEvaluateSubscripts[a_Integer] := a;
mEvaluateSubscripts[a_Symbol] := a;
mEvaluateSubscripts[a_String, b_String] := Symbol@StringJoin[a, b];
mEvaluateSubscripts[a_Symbol, b_Symbol] := Symbol@StringJoin[SymbolName@Unevaluated@a, b];
mEvaluateSubscripts[a_, b_] := Symbol@StringJoin[SymbolName@Unevaluated@a, SymbolName@Unevaluated@b]


Options[mAddQuantityInput] = {"FieldHint"->Null, "FieldSize"->8, "Checks"->{}};
SetAttributes[mAddQuantityInput, HoldFirst];
mAddQuantityInput[var_Symbol, p___] := mAddQuantityInput[Evaluate@SymbolName@Unevaluated@var, p];
mAddQuantityInput[var_String, unitType_, OptionsPattern[]] :=
NotebookWrite[EvaluationNotebook[],
	Needs["UserInterface`NotebookInterface`"];
	UserInterface`NotebookInterface`cellTemplateToBoxes[{
		UserInterface`NotebookInterface`setRowTemplateBoxes[var, {
			UserInterface`NotebookInterface`inputFieldTemplate[Null, Expression, (NumericQ@#) &, "must be a number", OptionValue["FieldHint"], FieldSize->OptionValue["FieldSize"]]
			,UserInterface`NotebookInterface`unitsMenuTemplate[Quantity[1, dfltUnit[unitType]], unitType]
		}]
	}]
];

mInlinePopupMenu[opts_List, startVal_: Undefined] :=
	Interpretation[{x = If[startVal === Undefined, First@opts, startVal]}, PopupMenu[Dynamic@x, opts], x];

mInlineCheckbox[startVal_: False] :=
	Interpretation[{x = startVal}, Checkbox[Dynamic@x], x];

SetAttributes[mInlineCompletion, HoldFirst];
mInlineCompletion[a_, startVal_: Undefined] :=
Module[{assoc = a, completionFunction, boxId=ToString@RandomInteger[9999999]},
	completionFunction[s_String] := TimeConstrained[ Select[Keys@assoc, StringStartsQ[#, s, IgnoreCase->True] &], .2];
	(*completionFunction2 = Function[{s}, TimeConstrained[ Select[Keys@assoc, StringStartsQ[#, s, IgnoreCase->True] &], .2];*)
	NotebookWrite[EvaluationNotebook[],
		Cell[BoxData@RowBox[{SymbolName@Unevaluated@a, "[",
						ToBoxes@Interpretation[{x = If[startVal === Undefined, Null, startVal]},
							InputField[Dynamic@x, String, BoxID->boxId, FieldCompletionFunction->completionFunction]
							, x
						]
					, "]"
			}]
			, "Input"
		]
	];
	mSetFocus[boxId];
];

mInlineDirectoryCompletion[startVal_:Null, opts:OptionsPattern[InputField]] :=
Module[{x},
Interpretation[{x = startVal},
	Row[{
		Framed[
			InputField[Dynamic@x, String, opts, Appearance->None, FieldCompletionFunction->fileCompletionFunction]
			, FrameMargins->1, FrameStyle->GrayLevel[0.7]
		]
		,Tooltip[
			Button["...",
				x = Replace[SystemDialogInput["Directory"], Except[_String] -> x];
				, Method->"Queued", ImageSize->{28, 26}, ImageMargins->0, FrameMargins->{{0, 0}, {1, 0}}
			]
			, "browse for directory"
		]
	}]
	, x
]];

fileCompletionFunction[x_String] /; And[x!="", FileExistsQ@x] := TimeConstrained[FileNames["*", StringReplace[x, "/"->"\\"]], 0.15, {}];
fileCompletionFunction[x_String] := TimeConstrained[FileNames[FileNameTake@x<>"*", DirectoryName@x], 0.15, {}];




(*** Notes to self about how funcs work ***)

(* Method methods *)
$mMethod = <|
	 FindRoot -> {"Brent"->"FindRoot[f[x], {x, lBound, rBound}] : one side +, one - ... bisect and repeat til near 0", "Secant"->"FindRoot[f[x], {x, x0}] : has trouble with discontinuities in slope", "Newton"->"FindRoot[f[x], {x, x0}] : has trouble with discontinuities in slope", Automatic}
	,FindMaximum -> {"PrincipalAxis"->"FindMaximum[f[x], {x, x0}] - the best!", "Gradient", "ConjugateGradient", "InteriorPoint", "QuasiNewton", "Newton", "LinearProgramming", "QuadraticProgramming", "LevenbergMarquardt", Automatic}
	,NMaximize -> {"DifferentialEvolution", "NelderMead", "RandomSearch", "SimulatedAnnealing", Automatic}
|>;


End[];

EndPackage[];

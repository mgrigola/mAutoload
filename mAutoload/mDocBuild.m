BeginPackage["mAutoload`mDocBuild`", {
	"mAutoload`"
}];
(*ClearAll["mAutoload`mDocBuild`*"];
ClearAll["mAutoload`mDocBuild`Private`*"];*)


mBuildUI::usage = "mBuildUI opens the UI";

mDocBuild::usage = "mDocBuild[baseDir, packageName] builds standard CBI documentation for package in baseDir, without eclipse/Ant. Like build.xml.deploy";
mDeploy::usage = "mDeploy[baseDir, deployDir, packageName] copies build & source files for package in baseDir to deployDir, without eclipse/Ant. Like build.xml.deployCompleted";

mParseAntXMLFile::usage = "mParseAntXMLFile[buildFile, packageName, (deployDir)] initially reads/parses build.xml to extract (& return) tasks/targets, top-level properties, etc. But doesn't do anything";
mExecuteAntTarget::usage = "mExecuteAntTarget[store, target] call with store = return value from mParseAntXMLFile to run the build.xml for the target";

replacePropVal::missingProp = "property not defined? `1`";
parseAntXMLElement::noPropertyVal = "no value for property? `1`";

Begin["`Private`"];


lookup[a:(_Association|_List), v_] := Lookup[a, v, Null];
lookup[___] := Null;

addTrailingSlash[s_] := If[StringEndsQ[s, "\\"], s, s <> "\\"];


(* initially read the XML file and store the 'top-level' info in a form we can work with later *)
(* we store the top-level properties ("properties"), Ant tasks ("tasks"), MMA initialization ("MMA") in Association. Parsing doesn't take action, execute** will do that *)
Options[mParseAntXMLFile] = {"deployDir"->Null};
mParseAntXMLFile[buildFile_, packageName_, OptionsPattern[]] :=
	Module[{xmlData,xmlElements,store},
		store = <|
			"targets" -> <||>,
			"properties" -> <|
				"mathematicaInstallDir" -> $InstallationDirectory,
				"packageName" -> packageName,
				"appPath.default" -> "",
				"deployDir" -> If[StringQ@OptionValue["deployDir"], OptionValue["deployDir"], "" (* $mDev?*)],
				"basedir" -> DirectoryName@buildFile
			|>
		|>;
		
		xmlData = Import[buildFile, "XML"][[2]];
		xmlElements = xmlData[[3]];
		parseAntXMLElement[store, #] & /@ xmlElements;
		store
	];


(* assumes store has key "properties" with all properties defined above this level. Can use Block-like-stuff to keep properties from bleeding *)
SetAttributes[{replaceProps, replacePropVal}, HoldFirst];
replaceProps[store_, str_String] :=
	StringReplace[str, RegularExpression["\\$\\{(.*?)}"] :> replacePropVal[store, "$1"]]

replacePropVal[store_, propStr_String] :=
	Lookup[store["properties"], propStr, Message[replacePropVal::missingProp, propStr]; propStr<>":$Failed"];



(* start to parse build.xml, sorta recursive, will put the interesting xml data into an association*)
SetAttributes[parseAntXMLElement, HoldFirst];
parseAntXMLElement[store_, XMLElement["property", rules_, _]] :=
	Module[{name = lookup[rules, "name"], val},
		If[name === Null, Return[]]; (* ignore the build.properties file at the moment *)
		val = lookup[rules, "value"];
		If[val === Null, Message[parseAntXMLElement::noPropertyVal, name]; Return[$Failed]];
		If[!KeyExistsQ[store, "properties"], store["properties"] = <||>];
		store["properties"][name] = replaceProps[store, val];
	];

parseAntXMLElement[store_, XMLElement["target", rules_, code_]] :=
	With[{name = Lookup[rules, "name", ToString@Unique[]]},
		If[!KeyExistsQ[store, "targets"], store["targets"] = <||>];
		store["targets"][name] = <|
			"depends" ->lookup[rules, "depends"],
			"code" -> code (* just gather the code, will be parsed/executed later *)
		|>;
		
	];

parseAntXMLElement[store_, XMLElement["mathematica", rules_, code_]] := (store["MMA"] = code);


(* execute (Ant Run) a target. Must have already parsed file. This wraps with the Block and junk once, then business happens in Impl *)
SetAttributes[{executeAntTargetImpl,executeAntXMLElement}, HoldFirst];

mExecuteAntTarget[buildFile_, packageName_, target_, opts:OptionsPattern[mParseAntXMLFile]] := mExecuteAntTarget[mParseAntXMLFile[buildFile, packageName, opts], target];
mExecuteAntTarget[Istore_, target_] :=
	Block[{store = Istore(*, AntProperty, AntLog, initialized = False, $Path = $Path*) , $origPath=System`$Path}, (* I guess this Block doesn't work inside a Package. It works in a notebook *)
		initialized = False;
		(*Needs["System`"]; (* issues with $Path in here. i dunno if this helps *)*)
		SetDirectory[store["properties","basedir"] ]; (* do this? file copy/delete relies on basedir a lot *)
		PrependTo[CurrentValue[$FrontEndSession, NotebookPath], store["properties","basedir"]]; (* allows relative-path NotebookOpen, which many buildPalette's have for _ reason..? palette will open & not popup error notebook.. *)
		executeAntTargetImpl[store, target];
		ResetDirectory[];
		CurrentValue[$FrontEndSession, NotebookPath] = Drop[CurrentValue[$FrontEndSession, NotebookPath], 1];
		System`$Path = $origPath;
		True
	];

executeAntTargetImpl[store_, target_] :=
With[{depends = lookup[store["targets",target], "depends"]},
	If[StringQ@depends, executeAntTargetImpl[store, depends] ];
	executeAntXMLElement[store, #]& /@ store["targets",target,"code"];
];

executeAntXMLElement[store_, XMLElement["echo", rules_, code_]] :=
	Print[ replaceProps[store, "message"/.rules] ] 

executeAntXMLElement[store_, p : XMLElement["property", rules_, code_]] :=
	parseAntXMLElement[store, p];

executeAntXMLElement[store_, XMLElement["mkdir", rules_, code_]] :=
With[{dir = lookup[rules, "dir"] },
	If[dir=!=Null, Quiet@CreateDirectory[replaceProps[store, dir] ]]
];

executeAntXMLElement[store_, XMLElement["delete", rules_, code_]] :=
Module[{file = lookup[rules, "file"], dir = lookup[rules, "dir"], fileset},
	If[file =!= Null,
		(*Print["Deleting file: ", replaceProps[store, file]]; Return[];*)
		Quiet@DeleteFile[replaceProps[store, file]]
	];
	If[dir =!= Null,
		(*Print["Deleting dir: ", replaceProps[store, dir]]; Return[];*)
		Quiet@DeleteDirectory[replaceProps[store, dir], DeleteContents -> True]
	];
	If[Length@code > 0,
		Quiet@Do[
			fileset = getAntXMLFileset[store, filesetXML];
			(*Print["Deleting FILESET: ", fileset]; Return[];*)
			If[DirectoryQ@#, DeleteDirectory[#, DeleteContents->True], DeleteFile@#]& /@ fileset["files"];
			, {filesetXML, code}
		];
	];
];

executeAntXMLElement[store_, XMLElement["copy", rules_, code_]] :=
Module[{toDir = lookup[rules, "todir"], file = lookup[rules, "file"], dir = lookup[rules, "dir"], src, fileset, tgt},
	toDir = replaceProps[store, toDir];
	If[DirectoryQ[StringJoin["./", toDir]],
		toDir = addTrailingSlash@FileNameJoin[{store["properties", "basedir"], toDir}];
	];
	
	If[file =!= Null,
		src = replaceProps[store, file];
		tgt = FileNameJoin[{toDir, FileNameTake[src, -1]}];
		(*Print["Copying file from: ", src, "  ->\n  ", FileNameJoin[{toDir, FileNameTake[src, -1]}]]; Return[];*)
		Quiet@CreateDirectory[toDir];
		CopyFile[src, tgt, OverwriteTarget->True]
	];
	If[dir =!= Null,
		src = replaceProps[store, dir];
		(*Print["Copying dir from: ", src, "  ->\n  ", toDir]; Return[];*)
		Quiet@CreateDirectory[DirectoryName@toDir];
		CopyDirectory[src, toDir]
	];
	If[Length@code > 0,
		Do[
			fileset = getAntXMLFileset[store, filesetXML];
			(*Print["Copying FILESET from: ", fileset, "  ->\n  ", fullTarget]; Return[];*)
(*			If[!DirectoryQ@#,
				Print["CopyFile[",
					#,
					" -> ",
					FileNameJoin[{toDir, StringTrim[#, fileset["filesetBase"]] }]
				] 
			]& /@ fileset["files"];*)
			
			If[!DirectoryQ@#,
				tgt = FileNameJoin[{toDir, StringTrim[#, fileset["filesetBase"]] }];
				Quiet@CreateDirectory[DirectoryName@tgt];
				CopyFile[#, tgt, OverwriteTarget->True]
			]& /@ fileset["files"];
			, {filesetXML, code}
		]
	];
];

executeAntXMLElement[store_, XMLElement["antcall", rules_, code_]] :=
With[{tgt=lookup[rules, "target"]},
	If[StringQ@tgt, executeAntTargetImpl[store, tgt]];
];

(* runs mathematica. Will execute the store["MMA"] ("mathematica" task) first. But doesn't care about jlink and stuff *)
executeAntXMLElement[store_, XMLElement["mathematica", rules_, code_]] :=
(
	Print["running mathematica"];
	Global`AntProperty = store["properties"];
	
	(* run the MMA initialization once. But Needs won't reload. You'd need to do like the incremental deploy with reload to Get any changed files. Awkward... Or Quit kernel *)
	If[!initialized,
		Print["running MMA init"];
		Global`AntLog = Print["AntLog: ", ##] &;
(*		Print["Path0: ", $Path];
		Print["System`Path0: ", System`$Path];*)
		PrependTo[System`$Path, Global`$$MMALibPath]; (* the $Path stuff just doesn't work inside a package like this. Tried Block, AllKindsOfContext`$Path, whatever. Just cheat and assume this is what store["MMA"] does.. *)
		PrependTo[System`$Path, Global`AntProperty["basedir"]];
		ToExpression /@ store["MMA"];
(*		Print["Path: ", $Path];
		Print["System`Path: ", System`$Path];*)
		initialized = True;
	];
(*	Global`$mypath2 = Global`$Path;
	Global`$mypath3 = mAutoload`$Path;
	Global`$mypath4 = mAutoload`mDocBuild`$Path;
	Global`$mypath5 = mAutoload`mDocBuild`Private`$Path;
	
	Global`$mypath0 = System`$Path;*)
	ToExpression /@ code; (* evaluates that CDATA mathematica code *)
(*	Global`$mypath1 = System`$Path;*)
);


(* this isn't perfect. not sure of the full rules for Ant *)
(* usually there's only include or exclude. Both might/not make sense? *)
(* if there are several in/ex-cludes, they appear appear as elements below fileset (my code_) *)
(* but I'm allowing both inline and element-below in/ex-cludes and lumpng together *)
(* the ** name syntax is a little sketchy. I guess if it starts with "**" that = FileNames[_, dir, Infinity] *)
SetAttributes[getAntXMLFileset, HoldFirst];
getAntXMLFileset[store_, XMLElement["fileset", rules_, code_]] :=
Module[{filesetBaseDir, files, includes, exclude, filesetStore},
	filesetStore = <|"properties" -> store["properties"], "includes" -> {}, "exclude" -> {}|>;
	filesetBaseDir = lookup[rules, "dir"];
	If[filesetBaseDir === Null, Message[getAntXMLFileset::noDir, rules]; Return[{}]];
	filesetBaseDir = replaceProps[filesetStore, filesetBaseDir];
	
	(* we need the absolute path to later strip out the absolute path that FileNames returns. But "dir" property may be absolute or relative, so this weird check to see which we have and get the absolute *)
	If[DirectoryQ@StringJoin["./", filesetBaseDir],
		filesetBaseDir = FileNameJoin[{store["properties","basedir"], filesetBaseDir}];
	];
	
	includes = Lookup[rules, "includes", Null];
	exclude = Lookup[rules, "exclude", Null];
	If[includes =!= Null, AppendTo[filesetStore["includes"], replaceProps[filesetStore, includes]]];
	If[exclude =!= Null, AppendTo[filesetStore["exclude"], replaceProps[filesetStore, exclude]]];
	getAntXMLFileset[filesetStore, #] & /@ code;
	
	(* make function instead of Join@@Map *)
	files = If[Length@filesetStore["includes"] > 0,
		Join @@ Map[antFileInclude[filesetBaseDir, #]&, filesetStore["includes"]]
		,
		FileNames["*", filesetBaseDir, Infinity]
	];
	
	If[Length@filesetStore["exlude"] > 0,
		files = antFileExclude[filesetBaseDir, filesetStore["exlude"], files]
	];
	
	<|"filesetBase" -> addTrailingSlash@filesetBaseDir, "files" -> files|>
];

getAntXMLFileset[filesetStore_, XMLElement["exclude", rules_, code_]] :=
With[{set = Lookup[rules, "name", Null]},
	(* does need Join getAntXMLFileset/@code   too? *)
	If[set =!= Null, AppendTo[filesetStore["exclude"], replaceProps[filesetStore, "name" /. rules]]]
];

getAntXMLFileset[filesetStore_, XMLElement["includes", rules_, code_]] :=
With[{set = Lookup[rules, "name", Null]},
	(* does need Join getAntXMLFileset/@code   too? *)
	If[set =!= Null, AppendTo[filesetStore["includes"], replaceProps[filesetStore, "name" /. rules]]]
];

antFileInclude[srcDir_, pattern_] :=
Module[{depth = 1, mmaPattern},
	mmaPattern = StringReplace[pattern, {"**" -> "*", "/" -> "\\"}];
	If[StringStartsQ[mmaPattern, "*\\"], depth = Infinity; mmaPattern = StringDrop[mmaPattern, 2]];
	FileNames[mmaPattern, srcDir, depth]
];

antFileExclude[srcDir_, patterns_, includeFiles_] :=
Module[{mmaPattern},
	mmaPattern = Alternatives @@ Map[excludePattern[srcDir, #] &, patterns];
	DeleteCases[includeFiles, x_ /; StringMatchQ[x, mmaPattern]]
];

excludePattern[pattern_String] :=
Apply[StringExpression, StringSplit[StringReplace[pattern, {"**" -> "*", "/" -> "\\"}], "*", All] /. "" -> BlankNullSequence[]];

excludePattern[srcDir_String, pattern_String] :=
Apply[StringExpression,
	Join[
		{BlankNullSequence[], addTrailingSlash@StringReplace[srcDir, "/" -> "\\"]},
		StringSplit[StringReplace[pattern, {"**" -> "*", "/" -> "\\"}], "*", All] /. "" -> BlankNullSequence[]
	]
];








(*AntLog = Null &;*)

(*For Symbol docs there are usually several menus separated by spaces. Drop or replace the URL and spaces before it*)
replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[{b__, "\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]",
Cell[BoxData[ActionMenuBox[FrameBox[InterpretationBox[Cell[TextData[{"URL", ___}], ___], ___], ___], ___]], ___]}], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___]] :=
	Cell[BoxData[GridBox[{{a, Cell[TextData[{b}], "AnchorBar"]}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[{b__, "\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]",
Cell[BoxData[ActionMenuBox[FrameBox[InterpretationBox[Cell[TextData[{"URL", ___}], ___], ___], ___], ___]], ___]}], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___], new_Cell] :=
	Cell[BoxData[GridBox[{{a, Cell[TextData[{b, "\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]", new}], "AnchorBar"]}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

(*Add pattern for old workbench plugin, which doesn't use InterpretationBox*)
replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[{b__, "\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]",
Cell[BoxData[ActionMenuBox[FrameBox["\"URL »\"", ___], ___]], ___]}], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___]] :=
	Cell[BoxData[GridBox[{{a, Cell[TextData[{b}], "AnchorBar"]}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[{b__, "\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]",
Cell[BoxData[ActionMenuBox[FrameBox["\"URL »\"", ___], ___]], ___]}], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___], new_Cell] :=
	Cell[BoxData[GridBox[{{a, Cell[TextData[{b, "\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]\[ThickSpace]", new}], "AnchorBar"]}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

(*For Tutorial docs there is usually only the URL menu. Drop or replace it*)
replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[Cell[BoxData[ActionMenuBox[FrameBox[InterpretationBox[
Cell[TextData[{"URL", ___}], ___], ___], ___], ___]], ___]], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___]] :=
	Cell[BoxData[GridBox[{{a}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[Cell[BoxData[ActionMenuBox[FrameBox[InterpretationBox[
Cell[TextData[{"URL", ___}], ___], ___], ___], ___]], ___]], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___], new_Cell] :=
	Cell[BoxData[GridBox[{{a, Cell[TextData[new], "AnchorBar"]}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

(*Add pattern for old workbench plugin, which doesn't use InterpretationBox*)
replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[Cell[BoxData[ActionMenuBox[FrameBox["\"URL »\"", ___], ___]], ___]], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___]] :=
	Cell[BoxData[GridBox[{{a}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

replaceURL[Cell[BoxData[GridBox[{{a_Cell, Cell[TextData[Cell[BoxData[ActionMenuBox[FrameBox["\"URL »\"", ___], ___]], ___]], "AnchorBar"]}}]], "AnchorBarGrid", opts___, CellID -> 1, opts2___], new_Cell] :=
	Cell[BoxData[GridBox[{{a, Cell[TextData[new], "AnchorBar"]}}]], "AnchorBarGrid", opts, CellID -> 1, opts2];

replaceURL[___] := $Failed;

printButton = Cell[BoxData[ButtonBox["\<\"Print\"\>", Appearance -> "Palette", ButtonFunction :> FrontEndExecute[FrontEndToken["PrintDialog"]], Evaluator -> Automatic, Method -> "Preemptive"]], "AnchorPrintButton"];



(* combine this with Wolfram save notebook function so don't need to visibly open the file twice *)


(* todo: System`FEDump`notebookPutMakeNotebook[]  +  Export[nbOcject, file]; ?? *)

(* This is different than the build.xml: we operate on the Notebook instead of the NotebookObject *)
(* NotebookObject is a FrontEnd entity (a visible, open notebook) ... Notebook is more like a list, not a window *)
(* Notebook = NotebookGet[NotebookObject]  ... NotebookObject = NotebookPut[Notebook] *)
(* Notebook can be saved with Export[path, Notebook] ... equivalent to NotebookSave[NotebookObject, path] *)
ClearAll[modifyCBIDocumentation];
modifyCBIDocumentation[nb_Notebook, outfile_String] :=
Module[{nbNew = nb},
	(* do print button thing for CDF doc notebooks? I've never actually seen this *)
	If[StringEndsQ[FileNameTake[outfile, -1], "CDF.nb"],
		nbNew[[1, 1]] = replaceURL[nbNew[[1, 1]], printButton];
		,
		nbNew[[1, 1]] = replaceURL[nbNew[[1, 1]]];
	];
	
	(* set stylesheet, like SetOptions[nbNewObject, StyleDefinitions->CBI_Doc] *)
	nbNew = nbNew /. Rule[StyleDefinitions, ___] :> Rule[StyleDefinitions, "CBI_Documentation/CBIDocumentation.nb"];
	
	Quiet@CreateDirectory[DirectoryName@outfile];
	Export[outfile, nbNew];
];



(*
	See ...\0\.cp\MathematicaSource\DocumentationBuild\*
	.\SystemFiles\ant\Build\notebook.xml - main thing run via Ant. called by Application Tools > Build
	notebook.xml sets some stuff up and ultimately calls DocumentationBuild`Make`MakeNotebook in .\Make.m
	
	the built-in code requires some modification to circumvent the Ant/workbench parts
	plus we can simplify with FrontEnd already open & combine CBI post-processing in single step
*)


ApplyConvertGraphicsToBitmapsToNotebook::error = "called this function (bad) for: `1`";


(* override built-in DocumentationBuild` functions *)
(* PubsEvaluateWithFE starts the FrontEnd assuming this is MathLink called from Ant (it's not) *)
(* SaveConvertGraphicsToBitmapsToNotebook converts Output cell graphics to bitmap if bitmap is smaller than graphics data. I'm not a fan. That's supposed to speed things up but it's a lie *)
(* instead route the notebook object to our post-processing so we don't need to close the file and open it again. We'll do 1 save to /build in modifyCBIDocumentation *)
(* could use Block to not permanently change the built-in functions, but eclipse/Ant build runs in separate kernel anyway *)

DocumentationBuild`Common`PubsEvaluateWithFE[expr___] := expr;

DocumentationBuild`Utils`SaveConvertGraphicsToBitmapsToNotebook[outfile_String, exp_Notebook, entityType_String, uri_String, opts___?OptionQ] := (
	modifyCBIDocumentation[NotebookPut@exp, outfile]
);

DocumentationBuild`Utils`ApplyConvertGraphicsToBitmapsToNotebook[exp_Notebook, entityType_String, uri_String, opts___?OptionQ] := Message[ApplyConvertGraphicsToBitmapsToNotebook::error, uri];


Options[mDocBuild] = {"IncludeLinkTrails"->False, "Incremental"->False};
mDocBuild[baseDir_String, opts : OptionsPattern[]] := mDocBuild[baseDir, StringTrim[FileNameTake[baseDir, -1], "Project"], opts];
mDocBuild[baseDir_String, packageName_String, OptionsPattern[]] :=
Module[{inputDir,inputFiles,outputDir,outputFile,linkBase,language,includeLinkTrails,incremental,guidesDir,indexDir,symbolsDir,spellIndexDir,tutorialsDir,newcoreinfo,today,inTime,outTime,nb,info,res,metaData,plainText},
	(*AppendTo[$Path,appPathDefault];*) (* DocumentationBuild also lives here. but it's v12-specific using PacletDirectoryLoad *)
	Needs["DocumentationBuild`"]; (* C:/ProgramData/Mathematica/Applications/DocumentationBuild -- possibly this was downloaded with v12 or one of the workbench updates? or maybe you can PacletDirectoryAdd appPathDefault *)
	Needs["DocumentationSearch`"]; (* %mmaInstallDir%/Addons/Applications/DocumentationSearch *)
	
	linkBase = packageName; (* e.g. SuspendedDeckDesign *)
	language = "English";
	includeLinkTrails = OptionValue["IncludeLinkTrails"];
	incremental = OptionValue["Incremental"]; (* by default will do incremental build, only rebuild input notebooks edited after output notebook was created *)
	today = ToString@Date[];
	
	inputDir = FileNameJoin[{baseDir, packageName, "Documentation"}];
	outputDir = FileNameJoin[{baseDir, "build", packageName, "Documentation"}];
	
	(* not sure if these folders get created upon outputting file.. probably don't need this chunk *)
	guidesDir = FileNameJoin[{outputDir, language, "Guides"}];
	indexDir = FileNameJoin[{outputDir, language, "Index"}];
	symbolsDir = FileNameJoin[{outputDir, language, "ReferencePages", "Symbols"}];
	spellIndexDir = FileNameJoin[{outputDir, language, "SpellIndex"}];
	tutorialsDir = FileNameJoin[{outputDir, language, "Tutorials"}];
	If[! incremental,
		Quiet@DeleteDirectory[guidesDir, DeleteContents->True];
		Quiet@DeleteDirectory[indexDir, DeleteContents->True];
		Quiet@DeleteDirectory[symbolsDir, DeleteContents->True];
		Quiet@DeleteDirectory[spellIndexDir, DeleteContents->True];
		Quiet@DeleteDirectory[tutorialsDir, DeleteContents->True];
	];
	Quiet@CreateDirectory[guidesDir];
	Quiet@CreateDirectory[indexDir];
	Quiet@CreateDirectory[spellIndexDir];
	
	(* the indexer is a Java package (Lucene?). This starts jvm and loads class *)
	DocumentationBuild`indexer = DocumentationSearch`NewDocumentationNotebookIndexer[indexDir];
	
	(* pretty sure this Block is irrelevant for our custom doc. Only for Wolframs's symbols? *)
	Block[{Print = Null &, mathsourceFile,mathsourcelist,packagesourcelist},
		mathsourceFile = ToFileName[{DocumentationBuild`Common`$DocumentationBuildDirectory, "Internal", "data"}, StringJoin["SourceInformation.", Switch[language, "Japanese", "ja", "ChineseSimplified", "zh", "Spanish", "es", _, "en"], ".m"]];
		mathsourcelist = Get[mathsourceFile];
		packagesourcelist = DocumentationBuild`Info`CreateSourceInformation[inputDir, language, linkBase];
		
		newcoreinfo = Join[DeleteCases[mathsourcelist, Alternatives @@ (Intersection[First /@ mathsourcelist, First /@ packagesourcelist]) -> _], packagesourcelist];
		DocumentationBuild`Navigation`$Pages = DocumentationBuild`Navigation`ComputeLinkTrails[guidesDir];
	];
	
	inputFiles = DocumentationBuild`Common`notebookFileNames[inputDir];
	Do[
		(* each iter builds 1 documentation notebook and (maybe) indexes it *)
		outputFile = FileNameJoin[{outputDir, Last@StringSplit[inputFile, inputDir]}];
		If[incremental,
			inTime = FileDate@inputFile;
			outTime = Quiet@Check[FileDate@outputFile, inTime - Quantity[1, "Days"]];
			,
			inTime = 1;
			outTime = 0;
		];
		
		nb = DocumentationBuild`Utils`GetQuietly[inputFile];
		info = DocumentationBuild`Info`GetNotebookInformation[nb];
		
		If[outTime < inTime,
			(* this one line is the whole build *)
			Quiet[{res, metaData} = DocumentationBuild`Make`MakeNotebook[nb, info, "outfile"->outputFile, "Language"->language, "CoreInformation"->newcoreinfo, "IncludeLinkTrails"->includeLinkTrails], {ReplaceAll::reps, First::nofirst}]
			,
			(* for incremental build: if already up-to-date, just get info for the indexer, no edit notebook *)
			res = outputFile;
			metaData = DocumentationBuild`Make`Private`ConvertValuesForSearch[{
					"built" -> today,
					"history" -> {"New", "Modified", "Obsolete", "Excised"} /. ("HistoryData" /. info),
					"context" -> ("Context" /. info),
					"keywords" -> ("Keywords" /. info),
					"specialkeywords" -> {},
					"tutorialcollectionlinks" -> ("TutorialCollectionLinks" /. info),
					"index" -> "IndexQ" /. info,
					"label" -> {},
					"language" -> DocumentationBuild`Utils`GetLanguageExtension[language],
					"paclet" -> ("PacletName" /. info),
					"status" -> ("Flag" /. info /. None -> "None"),
					(*"summary"->DocumentationBuild`Info`GetNotebookSummary[nb,("EntityType"/.info),("Title"/.info)],*) (* there's a comment about some bug and this line of code. Doesn't seem to matter *)
					"summary" -> ("Summary" /. info),
					"synonyms" -> ("Synonyms" /. info),
					"tabletags" -> {},
					"title" -> DocumentationBuild`Make`Private`ShortFormTitle[("Title" /. info)],
					"titlemodifier" -> ("TitleModifier" /. info),
					"windowtitle" -> ("Title" /. info),
					"type" -> ("EntityType" /. info),
					"uri" -> "URI" /. info
				}, language
			]
		];
		
		plainText = Import[outputFile, {"NB", "Plaintext"}];
		If[Head@plainText === String && Head@metaData === List,
			DocumentationSearch`AddDocumentationNotebook[DocumentationBuild`indexer, plainText, metaData];
		];
		
		, {inputFile, inputFiles}
	];
	
	(* call it twice because reasons? *)
	DocumentationSearch`CloseDocumentationNotebookIndexer[DocumentationBuild`indexer];
	DocumentationSearch`CloseDocumentationNotebookIndexer[DocumentationBuild`indexer];
	(* spellIndex usually fails because lock/can't delete. whatever... just be quiet *)
	Quiet@DocumentationSearch`CreateSpellIndex[indexDir, spellIndexDir];
	
	(* also copy PacletInfo to build because docbuild does that *)
	CopyFile[FileNameJoin[{baseDir, "PacletInfo.m"}], FileNameJoin[{baseDir, "build", packageName, "PacletInfo.m"}], OverwriteTarget->True];
];





copyFileOrDir[src_String?DirectoryQ, tgt_String] := CopyDirectory[src, tgt];
copyFileOrDir[src_String, tgt_String] := CopyFile[src, tgt, OverwriteTarget->True];

copyFileIncremental[src_String, tgt_String] :=
Module[{tgtTime, srcTime},
	tgtTime = Quiet@Check[FileDate@tgt, 0];
	If[tgtTime === 0,
		Quiet@CreateDirectory[DirectoryName@tgt];
		CopyFile[src, tgt, OverwriteTarget->True];
		,
		srcTime = FileDate@src;
		If[srcTime > tgtTime,
			CopyFile[src, tgt, OverwriteTarget->True]
		]
	]
];

copyFileIncrementalAndReload[src_String, tgt_String] :=
Module[{tgtTime, srcTime},
	tgtTime = Quiet@Check[FileDate@tgt, 0];
	If[tgtTime === 0,
		Quiet@CreateDirectory[DirectoryName@tgt];
		CopyFile[src, tgt, OverwriteTarget->True];
		,
		srcTime = FileDate@src;
		If[srcTime > tgtTime,
			CopyFile[src, tgt, OverwriteTarget->True];
			If[StringEndsQ[tgt, ".m"|".wl"],
				Get[tgt]
			];
		]
	]
];


Options[mDeploy] = {"Incremental"->False, "Reload"->Automatic};
mDeploy[baseDir_String, deployDir_String, opts:OptionsPattern[]] := mDeploy[baseDir, deployDir, StringTrim[FileNameTake[baseDir, -1], "Project"], opts];
mDeploy[baseDir_String, deployDir_String, packageName_String, OptionsPattern[]] :=
Module[{incremental, tgtDir, packageDir, buildDir, srcPackage, tgtPackage, srcBuild, tgtBuild, srcCDF, tgtCDF},
	incremental = OptionValue["Incremental"];
	packageDir = FileNameJoin[{baseDir, packageName}];
	tgtDir = FileNameJoin[{deployDir, packageName}];
	buildDir = FileNameJoin[{baseDir, "build", packageName}];
	
	(* TODO: often we (I) exclude like PackageName/(Palette)Resources/images/src ..or.. Resources/.../samples. Not happening here - there's build.xml parsing stuff above *)
	srcPackage = Select[FileNames["*", packageDir, 1], FileNameTake[#, -1] != "Documentation" &];
	tgtPackage = FileNameJoin[{tgtDir, Last@#}] & /@ StringSplit[srcPackage, packageDir];
	
	srcBuild = FileNames["*", buildDir, 1];
	tgtBuild = FileNameJoin[{tgtDir, Last@#}] & /@ StringSplit[srcBuild, buildDir];
	
	srcCDF = FileNames["*.cdf", ParentDirectory@buildDir, 1];
	tgtCDF = FileNameJoin[{deployDir, FileNameTake[#, -1]}] & /@ srcCDF;
	
	If[!incremental,
		Quiet@DeleteDirectory[tgtDir, DeleteContents->True];
		Quiet@CreateDirectory[tgtDir];
		
		MapThread[copyFileOrDir, {srcBuild, tgtBuild}];
		MapThread[copyFileOrDir, {srcPackage, tgtPackage} ];
		MapThread[copyFileOrDir, {srcCDF, tgtCDF}];
		,
		(* TODO: this is never worth it time-wise, but individual file names allows more complex rules like omit image src or samples... *)
		Module[{srcBuildFiles, tgtBuildFiles, srcPackageFiles, tgtPackageFiles},
			Quiet@CreateDirectory[tgtDir];
			
			srcBuildFiles = Flatten@Map[If[DirectoryQ@#, Select[FileNames["*", #, Infinity], DirectoryQ@# =!= True &], {#}] &, srcBuild];
			tgtBuildFiles = Map[FileNameJoin[{tgtDir, Last@StringSplit[#, buildDir]}] &, srcBuildFiles];
			
			srcPackageFiles = Flatten@Map[If[DirectoryQ@#, Select[FileNames["*", #, Infinity], DirectoryQ@# =!= True &], {#}] &, srcPackage];
			tgtPackageFiles = Map[FileNameJoin[{tgtDir, Last@StringSplit[#, packageDir]}] &, srcPackageFiles];
			
			(* if "Reload", also Get any changed source files. If Automatic (dflt) only if the target dir has been PacletDirectoryAdd'd *)
			If[TrueQ@OptionValue["Reload"] || And[ OptionValue["Reload"]===Automatic , MemberQ[PacletManager`PacletDirectoryAdd[], deployDir] ],
				MapThread[copyFileIncrementalAndReload, {srcPackageFiles, tgtPackageFiles}];
				,
				MapThread[copyFileIncremental, {srcPackageFiles, tgtPackageFiles}];
			];
			MapThread[copyFileIncremental, {srcBuildFiles, tgtBuildFiles}];
			MapThread[copyFileIncremental, {srcCDF, tgtCDF}];
		];
	];
];






forwardSlashes[x_String] := StringReplace[x, {"/\\"->"/", "\\"->"/"}];

PackageBuilder::no = "don't do that";


mBuildUI :=
DynamicModule[{
		dplyDir, deployDirs, gitDir, gitDirs, programNames, piFiles, buildType,
		task = "deploy", incremental = True(*, result = Null*)
	},
	
	If[!ListQ@gitDirs, gitDirs = {mAutoload`$mGitw, mAutoload`$mGit}];
	If[!ListQ@deployDirs, deployDirs = {mAutoload`$mDev, mAutoload`$mLib, mAutoload`$mAppDev(*,mAutoload`$mAppLib*)}];
	
	Column[{
		 Row[{
				Style["gitDir: ", FontFamily->"Open Sans"],
				Dynamic@PopupMenu[Dynamic@gitDir, gitDirs, Background->White, FieldSize->42]
				, Tooltip[Button["...",
					gitDir = Replace[SystemDialogInput["Directory"], Except[_String]->gitDir]; gitDirs = DeleteDuplicates@Append[gitDirs, gitDir];
					, Method->"Queued", ImageSize->{28, 26}, ImageMargins->2, FrameMargins->{{4, 4}, {0, 2}}]
					, "browse for git/working dir"
				]
				, Spacer[38]
				, Tooltip[
					Button["Add Git Paths",
						mAddPath[FileNameDrop[#, -1]] & /@ piFiles
						, ImageSize->120
					]
					, "add all working directories to $Path so you can Needs to load current version of any package"
				]
		}]
		,""
		,Dynamic[
			piFiles = StringReplace[#, {"/\\"->"/", "\\"->"/"}]& /@ FileNames["PacletInfo.m", gitDir, 3];
			programNames = With[{pacletInfos = Map[Association@@Get@# &, piFiles]}, KeyMap[ToString, #]["Name"]& /@ pacletInfos];
			Column[{
				 Row[{
						"deployDir: ",
						Dynamic@PopupMenu[Dynamic@dplyDir, deployDirs, Background->White, FieldSize->42]
						, Tooltip[Button["...",
								With[{dir = SystemDialogInput["Directory", If[StringQ@dplyDir, dplyDir, ""]]},
									Which[
										ToLowerCase@FileNameSplit@dir == {"\\\\mcdcorp\\cbi", "eng", "applications", "libraries"}, Message[PackageBuilder::no]
										, StringQ@dir, dplyDir = dir; deployDirs = DeleteDuplicates@Append[deployDirs, dplyDir]
										, True, Null
								]];
							, Method->"Queued", ImageSize->{28, 26}, ImageMargins->2, FrameMargins->{{4, 4}, {0, 2}}]
							, "browse for deploy dir"
						]
						, Spacer[16]
						, Button["Open deployDir", SystemOpen@dplyDir, ImageSize->120]
				}]
				,Row[{
						"Task: "
						,Spacer[5]
						,PopupMenu[Dynamic@task, {
								 "mDocBuild" -> Tooltip["mDocBuild", "Run MMA Build + CBI Post-processing on documentation files is working directory andput result in /build"]
								,"mDeploy" -> Tooltip["mDeploy", "Copies source files, documentation, & resources from working directory(/build/) to deployDir"]
								,"deploy" -> Tooltip["deploy", "run ant task if present in build.xml"] (* anything hitting docBuild is gonna fail. I dunno how to do <ant antfile="docbuild.xml" target="main"/> *)
								,"deployCompleted" -> Tooltip["deployCompleted", "run ant task if present in build.xml"]
								,"buildPalette" -> Tooltip["buildPalette", "run ant task if present in build.xml"]
								,"buildCDF" -> Tooltip["buildCDF", "run ant task if present in build.xml"]
								,"buildMainPkg" -> Tooltip["buildMainPkg", "run ant task if present in build.xml"]
								,"runTests" -> Tooltip["runTests", "run ant task if present in build.xml"]
								,"validationReport" -> Tooltip["validationReport", "run ant task if present in build.xml"]
							}
						]
						,Spacer[24]
						,Tooltip["Incremental: ",
							Column[{
								 "mDeploy: copies individual files, not directories. Use if some files locked - only those files won't copy"
								,"mDocBuild: only (re)builds notebooks updated since /build/ file was last edited"
							}]
						]
						,Checkbox[Dynamic@incremental]
(*						,Spacer[24]
						,Tooltip[RadioButtonBar[Dynamic@buildType, {"Generic Task", "Read build.xml"}, Enabled->False], "Generic does 'standard' build (move standard files from git to deploy)\nRead xml will attempt to parse the build.xml file (perform file renamings in deployCompleted, etc)"]*)
				}]
				,""
				,Multicolumn[
					MapThread[
						Button[#1,
							Switch[task,
								 "mDocBuild", mDocBuild[DirectoryName@#2, #1, "Incremental"->incremental]
								,"mDeploy", mDeploy[DirectoryName@#2, dplyDir, #1, "Incremental"->incremental]
								,_, mExecuteAntTarget[FileNameJoin[{DirectoryName@#2, "build.xml"}], #1, task, "deployDir"->dplyDir]
							]
							, Method->"Queued"
						] &
						, {programNames, piFiles}
					]
					, 4
				]
				}
				, BaseStyle->{FontFamily->"Open Sans", FontSize->12}, Spacings->{Automatic, {0, 1, .5, 0}}
			]
			, TrackedSymbols :> {gitDir}
		]
	}]
]




End[];

EndPackage[];

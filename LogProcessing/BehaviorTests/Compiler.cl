;;;; Compile the source files.

(load "./Base/Utility.cl")
(load "./Base/Htime.cl")
(load "./Base/API.cl")
(load "./Base/CMD.cl")
(load "./Base/LogReader.cl")
(load "./Base/CMDReader.cl")
(load "./Base/StackProcessing.cl")
(load "./Base/LogTools.cl")
(load "./Base/ODBC.cl")
(load "./Base/TestSet.cl")

(load "./Tests/StackTests.cl")
(load "./Tests/CMDTests.cl")
(load "./Tests/ProcDepth.cl")
(load "./Tests/EntryPair.cl")

(load "./HDMain.cl")
(load "./Problems.cl")
(load "./Binning.cl")
(load "./Tests/Loader.cl")

(compile-file "./Base/Utility.cl")
(compile-file "./Base/Htime.cl")
(compile-file "./Base/API.cl")
(compile-file "./Base/CMD.cl")
(compile-file "./Base/LogReader.cl")
(compile-file "./Base/CMDReader.cl")
(compile-file "./Base/StackProcessing.cl")
(compile-file "./Base/LogTools.cl")
(compile-file "./Base/ODBC.cl")
(compile-file "./Base/TestSet.cl")

(compile-file  "./Tests/StackTests.cl")
(compile-file  "./Tests/CMDTests.cl")
(compile-file  "./Tests/ProcDepth.cl")
(compile-file  "./Tests/EntryPair.cl")

(compile-file "./HDMain.cl")
(compile-file "./Problems.cl")
(compile-file "./Binning.cl")


%% This file is generated; DO NOT EDIT MANUALLY.

-module(b_rel_references_SUITE).
-compile(export_all).
-include("ct.hrl").
-import(lists, [foreach/2, map/2]).

-define(print_error_or_return(Res, Testcase),
        case Res of
            true ->
                {test, ok};
            false ->
                io:format("EXPECTED:~n    ~p~nGOT:~n    ~p~nCONV:~n    ~p~n~n",
                          [E, G, conv_from_get(G)]),
                exit("FAIL: Mismatch in ~p in ~p~n", 
                     [Testcase, "b_rel_references_SUITE %>"])
        end).

-define(test(Func, Path, Ref, Expected),
        Func(_Config) ->
               E = Expected,
               G = hnget("/" ++ "b_rel_references" ++ Path, Ref),
               Res = cmp(G, E),
               ?print_error_or_return(Res, Func)).

%% TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
?test(sheet1_A1, "/Sheet1/", "A1", "This tests the basics of functions").
?test(sheet1_A2, "/Sheet1/", "A2", "Erlang Ref").
?test(sheet1_B2, "/Sheet1/", "B2", "Formula").
?test(sheet1_C2, "/Sheet1/", "C2", "Answer").
?test(sheet1_E2, "/Sheet1/", "E2", "Data And Databases").
?test(sheet1_M2, "/Sheet1/", "M2", "Notes").
?test(sheet1_A3, "/Sheet1/", "A3", " countif/2,").
?test(sheet1_B3, "/Sheet1/", "B3", 0.0).
?test(sheet1_E3, "/Sheet1/", "E3", "Data ->").
?test(sheet1_F3, "/Sheet1/", "F3", "{1,2,3}").
?test(sheet1_G3, "/Sheet1/", "G3", "{0,1,2}").
?test(sheet1_H3, "/Sheet1/", "H3", "bed").
?test(sheet1_I3, "/Sheet1/", "I3", "bad").
?test(sheet1_J3, "/Sheet1/", "J3", "bid").
?test(sheet1_A4, "/Sheet1/", "A4", " covar/2,").
?test(sheet1_B4, "/Sheet1/", "B4", -2.0).
?test(sheet1_E4, "/Sheet1/", "E4", "Data ->").
?test(sheet1_F4, "/Sheet1/", "F4", 12.0).
?test(sheet1_G4, "/Sheet1/", "G4", 13.0).
?test(sheet1_H4, "/Sheet1/", "H4", 14.0).
?test(sheet1_I4, "/Sheet1/", "I4", 12.0).
?test(sheet1_J4, "/Sheet1/", "J4", 7.0).
?test(sheet1_K4, "/Sheet1/", "K4", 6.0).
?test(sheet1_A5, "/Sheet1/", "A5", " covar/2,").
?test(sheet1_B5, "/Sheet1/", "B5", -3.0).
?test(sheet1_E5, "/Sheet1/", "E5", "Data ->").
?test(sheet1_F5, "/Sheet1/", "F5", 12.0).
?test(sheet1_H5, "/Sheet1/", "H5", 14.0).
?test(sheet1_I5, "/Sheet1/", "I5", 12.0).
?test(sheet1_J5, "/Sheet1/", "J5", 7.0).
?test(sheet1_K5, "/Sheet1/", "K5", 6.0).
?test(sheet2_A1, "/Sheet2/", "A1", "abs/1,").
?test(sheet2_A2, "/Sheet2/", "A2", " acos/1,").
?test(sheet2_A3, "/Sheet2/", "A3", " acosh/1,").
?test(sheet2_A4, "/Sheet2/", "A4", " address/5,").
?test(sheet2_A5, "/Sheet2/", "A5", " andf/1,").
?test(sheet2_A6, "/Sheet2/", "A6", " asc/1,").
?test(sheet2_A7, "/Sheet2/", "A7", " areas/1,").
?test(sheet2_A8, "/Sheet2/", "A8", " asin/1,").
?test(sheet2_A9, "/Sheet2/", "A9", " asinh/1,").
?test(sheet2_A10, "/Sheet2/", "A10", " atan/1,").
?test(sheet2_A11, "/Sheet2/", "A11", " atan2/2,").
?test(sheet2_A12, "/Sheet2/", "A12", " atanh/1,").
?test(sheet2_A13, "/Sheet2/", "A13", " avedev/1,").
?test(sheet2_A14, "/Sheet2/", "A14", " average/1,").
?test(sheet2_A15, "/Sheet2/", "A15", " averagea/1,").
?test(sheet2_A16, "/Sheet2/", "A16", " betadist/5,").
?test(sheet2_A17, "/Sheet2/", "A17", " betainv/5,").
?test(sheet2_A18, "/Sheet2/", "A18", " binomdist/4,").
?test(sheet2_A19, "/Sheet2/", "A19", " calculate/2,").
?test(sheet2_A20, "/Sheet2/", "A20", " ceiling/2,").
?test(sheet2_A21, "/Sheet2/", "A21", " cell/2,").
?test(sheet2_A22, "/Sheet2/", "A22", " char/1,").
?test(sheet2_A23, "/Sheet2/", "A23", " chidist/2,").
?test(sheet2_A24, "/Sheet2/", "A24", " chiinv/2,").
?test(sheet2_A25, "/Sheet2/", "A25", " chitest/2,").
?test(sheet2_A26, "/Sheet2/", "A26", " choose/1,").
?test(sheet2_A27, "/Sheet2/", "A27", " clean/1,").
?test(sheet2_A28, "/Sheet2/", "A28", " code/1,").
?test(sheet2_A29, "/Sheet2/", "A29", " column/1,").
?test(sheet2_A30, "/Sheet2/", "A30", " columns/1,").
?test(sheet2_A31, "/Sheet2/", "A31", " combin/2,").
?test(sheet2_A32, "/Sheet2/", "A32", " concatenate/1,").
?test(sheet2_A33, "/Sheet2/", "A33", " confidence/3,").
?test(sheet2_A34, "/Sheet2/", "A34", " correl/2,").
?test(sheet2_A35, "/Sheet2/", "A35", " cos/1,").
?test(sheet2_A36, "/Sheet2/", "A36", " cosh/1,").
?test(sheet2_A37, "/Sheet2/", "A37", " count/1,").
?test(sheet2_A38, "/Sheet2/", "A38", " counta/1,").
?test(sheet2_A39, "/Sheet2/", "A39", " countblank/1,").
?test(sheet2_A40, "/Sheet2/", "A40", " countif/2,").
?test(sheet2_A41, "/Sheet2/", "A41", " covar/2,").
?test(sheet2_A42, "/Sheet2/", "A42", " critbinom/3,").
?test(sheet2_A43, "/Sheet2/", "A43", " date/3,").
?test(sheet2_A44, "/Sheet2/", "A44", " datevalue/1,").
?test(sheet2_A45, "/Sheet2/", "A45", " day/1,").
?test(sheet2_A46, "/Sheet2/", "A46", " days360/3,").
?test(sheet2_A47, "/Sheet2/", "A47", " daverage/4,").
?test(sheet2_A48, "/Sheet2/", "A48", " db/4,").
?test(sheet2_A49, "/Sheet2/", "A49", " db/5,").
?test(sheet2_A50, "/Sheet2/", "A50", " dcount/4,").
?test(sheet2_A51, "/Sheet2/", "A51", " dcounta/4,").
?test(sheet2_A52, "/Sheet2/", "A52", " ddb/4,").
?test(sheet2_A53, "/Sheet2/", "A53", " ddb/5,").
?test(sheet2_A54, "/Sheet2/", "A54", " devsq/1,").
?test(sheet2_A55, "/Sheet2/", "A55", " dget/4,").
?test(sheet2_A56, "/Sheet2/", "A56", " days360/2,").
?test(sheet2_A57, "/Sheet2/", "A57", " error_type/1,").
?test(sheet2_A58, "/Sheet2/", "A58", " even/1,").
?test(sheet2_A59, "/Sheet2/", "A59", " exact/2,").
?test(sheet2_A60, "/Sheet2/", "A60", " exp/1,").
?test(sheet2_A61, "/Sheet2/", "A61", " expondist/3,").
?test(sheet2_A62, "/Sheet2/", "A62", " fact/1,").
?test(sheet2_A63, "/Sheet2/", "A63", " false/0,").
?test(sheet2_A64, "/Sheet2/", "A64", " fdist/3,").
?test(sheet2_A65, "/Sheet2/", "A65", " find/2,").
?test(sheet2_A66, "/Sheet2/", "A66", " find/3,").
?test(sheet2_A67, "/Sheet2/", "A67", " findb/2,").
?test(sheet2_A68, "/Sheet2/", "A68", " findb/3,").
?test(sheet2_A69, "/Sheet2/", "A69", " finv/3,").
?test(sheet2_A70, "/Sheet2/", "A70", " fisher/1,").
?test(sheet2_A71, "/Sheet2/", "A71", " fisherinv/1,").
?test(sheet2_A72, "/Sheet2/", "A72", " fixed/1,").
?test(sheet2_A73, "/Sheet2/", "A73", " fixed/2,").
?test(sheet2_A74, "/Sheet2/", "A74", " fixed/3,").
?test(sheet2_A75, "/Sheet2/", "A75", " floor/2,").
?test(sheet2_A76, "/Sheet2/", "A76", " forecast/3,").
?test(sheet2_A77, "/Sheet2/", "A77", " frequency/2,").
?test(sheet2_A78, "/Sheet2/", "A78", " ftest/2,").
?test(sheet2_A79, "/Sheet2/", "A79", " fv/3,").
?test(sheet2_A80, "/Sheet2/", "A80", " fv/4,").
?test(sheet2_A81, "/Sheet2/", "A81", " fv/5,").
?test(sheet2_A82, "/Sheet2/", "A82", " gammadist/4,").
?test(sheet2_A83, "/Sheet2/", "A83", " gammainv/3,").
?test(sheet2_A84, "/Sheet2/", "A84", " gammaln/1,").
?test(sheet2_A85, "/Sheet2/", "A85", " geomean/1,").
?test(sheet2_A86, "/Sheet2/", "A86", " gestep/1,").
?test(sheet2_A87, "/Sheet2/", "A87", " gestep/2,").
?test(sheet2_A88, "/Sheet2/", "A88", " growth/4,").
?test(sheet2_A89, "/Sheet2/", "A89", " harmean/1,").
?test(sheet2_A90, "/Sheet2/", "A90", " hour/1,").
?test(sheet2_A91, "/Sheet2/", "A91", " hypgeomdist/4,").
?test(sheet2_A92, "/Sheet2/", "A92", " iff/1,").
?test(sheet2_A93, "/Sheet2/", "A93", " index/4,").
?test(sheet2_A94, "/Sheet2/", "A94", " indirect/1,").
?test(sheet2_A95, "/Sheet2/", "A95", " int/1,").
?test(sheet2_A96, "/Sheet2/", "A96", " intercept/2,").
?test(sheet2_A97, "/Sheet2/", "A97", " ipmt/4,").
?test(sheet2_A98, "/Sheet2/", "A98", " ipmt/5,").
?test(sheet2_A99, "/Sheet2/", "A99", " ipmt/6,").
?test(sheet2_A100, "/Sheet2/", "A100", " irr/1,").
?test(sheet2_A101, "/Sheet2/", "A101", " irr/2,").
?test(sheet2_A102, "/Sheet2/", "A102", " iserr/1,").
?test(sheet2_A103, "/Sheet2/", "A103", " iserror/1,").
?test(sheet2_A104, "/Sheet2/", "A104", " islogical/1,").
?test(sheet2_A105, "/Sheet2/", "A105", " isna/1,").
?test(sheet2_A106, "/Sheet2/", "A106", " isnontext/1,").
?test(sheet2_A107, "/Sheet2/", "A107", " isnumber/1,").
?test(sheet2_A108, "/Sheet2/", "A108", " ispmt/4,").
?test(sheet2_A109, "/Sheet2/", "A109", " istext/1,").
?test(sheet2_A110, "/Sheet2/", "A110", " kurt/1,").
?test(sheet2_A111, "/Sheet2/", "A111", " large/2,").
?test(sheet2_A112, "/Sheet2/", "A112", " left/1,").
?test(sheet2_A113, "/Sheet2/", "A113", " left/2,").
?test(sheet2_A114, "/Sheet2/", "A114", " leftb/1,").
?test(sheet2_A115, "/Sheet2/", "A115", " leftb/2,").
?test(sheet2_A116, "/Sheet2/", "A116", " len/1,").
?test(sheet2_A117, "/Sheet2/", "A117", " lenb/1,").
?test(sheet2_A118, "/Sheet2/", "A118", " linest/2,").
?test(sheet2_A119, "/Sheet2/", "A119", " linest/3,").
?test(sheet2_A120, "/Sheet2/", "A120", " ln/1,").
?test(sheet2_A121, "/Sheet2/", "A121", " log/1,").
?test(sheet2_A122, "/Sheet2/", "A122", " log/2,").
?test(sheet2_A123, "/Sheet2/", "A123", " log10/1,").
?test(sheet2_A124, "/Sheet2/", "A124", " lower/1,").
?test(sheet2_A125, "/Sheet2/", "A125", " max/1,").
?test(sheet2_A126, "/Sheet2/", "A126", " maxa/1,").
?test(sheet2_A127, "/Sheet2/", "A127", " mdeterm/1,").
?test(sheet2_A128, "/Sheet2/", "A128", " median/1,").
?test(sheet2_A129, "/Sheet2/", "A129", " mid/3,").
?test(sheet2_A130, "/Sheet2/", "A130", " midb/3,").
?test(sheet2_A131, "/Sheet2/", "A131", " min/1,").
?test(sheet2_A132, "/Sheet2/", "A132", " mina/1,").
?test(sheet2_A133, "/Sheet2/", "A133", " minute/1,").
?test(sheet2_A134, "/Sheet2/", "A134", " minverse/2,").
?test(sheet2_A135, "/Sheet2/", "A135", " mmult/4,").
?test(sheet2_A136, "/Sheet2/", "A136", " mod/2,").
?test(sheet2_A137, "/Sheet2/", "A137", " mode/1,").
?test(sheet2_A138, "/Sheet2/", "A138", " month/1,").
?test(sheet2_A139, "/Sheet2/", "A139", " n/1,").
?test(sheet2_A140, "/Sheet2/", "A140", " na/0,").
?test(sheet2_A141, "/Sheet2/", "A141", " notf/1,").
?test(sheet2_A142, "/Sheet2/", "A142", " normdist/4,").
?test(sheet2_A143, "/Sheet2/", "A143", " normsdist/1,").
?test(sheet2_A144, "/Sheet2/", "A144", " now/0,").
?test(sheet2_A145, "/Sheet2/", "A145", " odd/1,").
?test(sheet2_A146, "/Sheet2/", "A146", " orf/1,").
?test(sheet2_A147, "/Sheet2/", "A147", " permut/2,").
?test(sheet2_A148, "/Sheet2/", "A148", " pi/0,").
?test(sheet2_A149, "/Sheet2/", "A149", " pound/1,").
?test(sheet2_A150, "/Sheet2/", "A150", " pound/2,").
?test(sheet2_A151, "/Sheet2/", "A151", " power/2,").
?test(sheet2_A152, "/Sheet2/", "A152", " product/1,").
?test(sheet2_A153, "/Sheet2/", "A153", " radians/1,").
?test(sheet2_A154, "/Sheet2/", "A154", " rand/0,").
?test(sheet2_A155, "/Sheet2/", "A155", " replace/4,").
?test(sheet2_A156, "/Sheet2/", "A156", " rept/2,").
?test(sheet2_A157, "/Sheet2/", "A157", " right/1,").
?test(sheet2_A158, "/Sheet2/", "A158", " right/2,").
?test(sheet2_A159, "/Sheet2/", "A159", " rightb/1,").
?test(sheet2_A160, "/Sheet2/", "A160", " rightb/2,").
?test(sheet2_A161, "/Sheet2/", "A161", " round/2,").
?test(sheet2_A162, "/Sheet2/", "A162", " rounddown/2,").
?test(sheet2_A163, "/Sheet2/", "A163", " roundup/2,").
?test(sheet2_A164, "/Sheet2/", "A164", " search/2,").
?test(sheet2_A165, "/Sheet2/", "A165", " search/3,").
?test(sheet2_A166, "/Sheet2/", "A166", " searchb/2,").
?test(sheet2_A167, "/Sheet2/", "A167", " searchb/3,").
?test(sheet2_A168, "/Sheet2/", "A168", " second/1,").
?test(sheet2_A169, "/Sheet2/", "A169", " sign/1,").
?test(sheet2_A170, "/Sheet2/", "A170", " sin/1,").
?test(sheet2_A171, "/Sheet2/", "A171", " sinh/1,").
?test(sheet2_A172, "/Sheet2/", "A172", " small/2,").
?test(sheet2_A173, "/Sheet2/", "A173", " sqrt/1,").
?test(sheet2_A174, "/Sheet2/", "A174", " standardise/3,").
?test(sheet2_C174, "/Sheet2/", "C174", 174.0).
?test(sheet2_D174, "/Sheet2/", "D174", 0.790909090909091).
?test(sheet2_A175, "/Sheet2/", "A175", " stdev/1,").
?test(sheet2_C175, "/Sheet2/", "C175", 175.0).
?test(sheet2_D175, "/Sheet2/", "D175", 0.795454545454545).
?test(sheet2_A176, "/Sheet2/", "A176", " stdevp/1,").
?test(sheet2_C176, "/Sheet2/", "C176", 176.0).
?test(sheet2_D176, "/Sheet2/", "D176", 0.8).
?test(sheet2_A177, "/Sheet2/", "A177", " substitute/3,").
?test(sheet2_C177, "/Sheet2/", "C177", 177.0).
?test(sheet2_D177, "/Sheet2/", "D177", 0.804545454545455).
?test(sheet2_A178, "/Sheet2/", "A178", " substitute/4,").
?test(sheet2_C178, "/Sheet2/", "C178", 178.0).
?test(sheet2_D178, "/Sheet2/", "D178", 0.809090909090909).
?test(sheet2_A179, "/Sheet2/", "A179", " sum/1,").
?test(sheet2_C179, "/Sheet2/", "C179", 179.0).
?test(sheet2_D179, "/Sheet2/", "D179", 0.813636363636364).
?test(sheet2_A180, "/Sheet2/", "A180", " sumproduct/2,").
?test(sheet2_C180, "/Sheet2/", "C180", 180.0).
?test(sheet2_D180, "/Sheet2/", "D180", 0.818181818181818).
?test(sheet2_A181, "/Sheet2/", "A181", " sumsq/1,").
?test(sheet2_C181, "/Sheet2/", "C181", 181.0).
?test(sheet2_D181, "/Sheet2/", "D181", 0.822727272727273).
?test(sheet2_A182, "/Sheet2/", "A182", " tan/1,").
?test(sheet2_C182, "/Sheet2/", "C182", 182.0).
?test(sheet2_D182, "/Sheet2/", "D182", 0.827272727272727).
?test(sheet2_A183, "/Sheet2/", "A183", " tanh/1,").
?test(sheet2_C183, "/Sheet2/", "C183", 183.0).
?test(sheet2_D183, "/Sheet2/", "D183", 0.831818181818182).
?test(sheet2_A184, "/Sheet2/", "A184", " today/0,").
?test(sheet2_C184, "/Sheet2/", "C184", 184.0).
?test(sheet2_D184, "/Sheet2/", "D184", 0.836363636363636).
?test(sheet2_A185, "/Sheet2/", "A185", " trim/1,").
?test(sheet2_C185, "/Sheet2/", "C185", 185.0).
?test(sheet2_D185, "/Sheet2/", "D185", 0.840909090909091).
?test(sheet2_A186, "/Sheet2/", "A186", " true/0,").
?test(sheet2_C186, "/Sheet2/", "C186", 186.0).
?test(sheet2_D186, "/Sheet2/", "D186", 0.845454545454545).
?test(sheet2_A187, "/Sheet2/", "A187", " type/1,").
?test(sheet2_C187, "/Sheet2/", "C187", 187.0).
?test(sheet2_D187, "/Sheet2/", "D187", 0.85).
?test(sheet2_A188, "/Sheet2/", "A188", " upper/1,").
?test(sheet2_C188, "/Sheet2/", "C188", 188.0).
?test(sheet2_D188, "/Sheet2/", "D188", 0.854545454545454).
?test(sheet2_A189, "/Sheet2/", "A189", " value/1,").
?test(sheet2_C189, "/Sheet2/", "C189", 189.0).
?test(sheet2_D189, "/Sheet2/", "D189", 0.859090909090909).
?test(sheet2_A190, "/Sheet2/", "A190", " var/1,").
?test(sheet2_C190, "/Sheet2/", "C190", 190.0).
?test(sheet2_D190, "/Sheet2/", "D190", 0.863636363636364).
?test(sheet2_A191, "/Sheet2/", "A191", " varp/1,").
?test(sheet2_C191, "/Sheet2/", "C191", 191.0).
?test(sheet2_D191, "/Sheet2/", "D191", 0.868181818181818).
?test(sheet2_A192, "/Sheet2/", "A192", " year/1,").
?test(sheet2_C192, "/Sheet2/", "C192", 192.0).
?test(sheet2_D192, "/Sheet2/", "D192", 0.872727272727273).
?test(sheet2_A193, "/Sheet2/", "A193", " percentile/2,").
?test(sheet2_C193, "/Sheet2/", "C193", 193.0).
?test(sheet2_D193, "/Sheet2/", "D193", 0.877272727272727).
?test(sheet2_A194, "/Sheet2/", "A194", " proper/1,").
?test(sheet2_C194, "/Sheet2/", "C194", 194.0).
?test(sheet2_D194, "/Sheet2/", "D194", 0.881818181818182).
?test(sheet2_A195, "/Sheet2/", "A195", " quartile/2,").
?test(sheet2_C195, "/Sheet2/", "C195", 195.0).
?test(sheet2_D195, "/Sheet2/", "D195", 0.886363636363636).
?test(sheet2_A196, "/Sheet2/", "A196", " rank/2,").
?test(sheet2_C196, "/Sheet2/", "C196", 196.0).
?test(sheet2_D196, "/Sheet2/", "D196", 0.890909090909091).
?test(sheet2_A197, "/Sheet2/", "A197", " rank/3,").
?test(sheet2_C197, "/Sheet2/", "C197", 197.0).
?test(sheet2_D197, "/Sheet2/", "D197", 0.895454545454546).
?test(sheet2_A198, "/Sheet2/", "A198", " rows/2,").
?test(sheet2_C198, "/Sheet2/", "C198", 198.0).
?test(sheet2_D198, "/Sheet2/", "D198", 0.9).
?test(sheet2_A199, "/Sheet2/", "A199", " pearson/2,").
?test(sheet2_C199, "/Sheet2/", "C199", 199.0).
?test(sheet2_D199, "/Sheet2/", "D199", 0.904545454545455).
?test(sheet2_A200, "/Sheet2/", "A200", " moment/2,").
?test(sheet2_C200, "/Sheet2/", "C200", 200.0).
?test(sheet2_D200, "/Sheet2/", "D200", 0.909090909090909).
?test(sheet2_A201, "/Sheet2/", "A201", " skew/1,").
?test(sheet2_C201, "/Sheet2/", "C201", 201.0).
?test(sheet2_D201, "/Sheet2/", "D201", 0.913636363636364).
?test(sheet2_A202, "/Sheet2/", "A202", " slope/2,").
?test(sheet2_C202, "/Sheet2/", "C202", 202.0).
?test(sheet2_D202, "/Sheet2/", "D202", 0.918181818181818).
?test(sheet2_A203, "/Sheet2/", "A203", " steyx/2,").
?test(sheet2_C203, "/Sheet2/", "C203", 203.0).
?test(sheet2_D203, "/Sheet2/", "D203", 0.922727272727273).
?test(sheet2_A204, "/Sheet2/", "A204", " stdeva/1,").
?test(sheet2_C204, "/Sheet2/", "C204", 204.0).
?test(sheet2_D204, "/Sheet2/", "D204", 0.927272727272727).
?test(sheet2_A205, "/Sheet2/", "A205", " subtotal/2,").
?test(sheet2_C205, "/Sheet2/", "C205", 205.0).
?test(sheet2_D205, "/Sheet2/", "D205", 0.931818181818182).
?test(sheet2_A206, "/Sheet2/", "A206", " sumif/2,").
?test(sheet2_C206, "/Sheet2/", "C206", 206.0).
?test(sheet2_D206, "/Sheet2/", "D206", 0.936363636363636).
?test(sheet2_A207, "/Sheet2/", "A207", " sumif/3,").
?test(sheet2_C207, "/Sheet2/", "C207", 207.0).
?test(sheet2_D207, "/Sheet2/", "D207", 0.940909090909091).
?test(sheet2_A208, "/Sheet2/", "A208", " sumx2my2/2,").
?test(sheet2_C208, "/Sheet2/", "C208", 208.0).
?test(sheet2_D208, "/Sheet2/", "D208", 0.945454545454545).
?test(sheet2_A209, "/Sheet2/", "A209", " sumx2py2/2,").
?test(sheet2_C209, "/Sheet2/", "C209", 209.0).
?test(sheet2_D209, "/Sheet2/", "D209", 0.95).
?test(sheet2_A210, "/Sheet2/", "A210", " sumxmy2/2,").
?test(sheet2_C210, "/Sheet2/", "C210", 210.0).
?test(sheet2_D210, "/Sheet2/", "D210", 0.954545454545455).
?test(sheet2_A211, "/Sheet2/", "A211", " trend/3,").
?test(sheet2_C211, "/Sheet2/", "C211", 211.0).
?test(sheet2_D211, "/Sheet2/", "D211", 0.959090909090909).
?test(sheet2_A212, "/Sheet2/", "A212", " trend/4,").
?test(sheet2_C212, "/Sheet2/", "C212", 212.0).
?test(sheet2_D212, "/Sheet2/", "D212", 0.963636363636364).
?test(sheet2_A213, "/Sheet2/", "A213", " transpose/2,").
?test(sheet2_C213, "/Sheet2/", "C213", 213.0).
?test(sheet2_D213, "/Sheet2/", "D213", 0.968181818181818).
?test(sheet2_A214, "/Sheet2/", "A214", " trimmean/2,").
?test(sheet2_C214, "/Sheet2/", "C214", 214.0).
?test(sheet2_D214, "/Sheet2/", "D214", 0.972727272727273).
?test(sheet2_A215, "/Sheet2/", "A215", " trunc/1,").
?test(sheet2_C215, "/Sheet2/", "C215", 215.0).
?test(sheet2_D215, "/Sheet2/", "D215", 0.977272727272727).
?test(sheet2_A216, "/Sheet2/", "A216", " trunc/2,").
?test(sheet2_C216, "/Sheet2/", "C216", 216.0).
?test(sheet2_D216, "/Sheet2/", "D216", 0.981818181818182).
?test(sheet2_A217, "/Sheet2/", "A217", " vara/1,").
?test(sheet2_C217, "/Sheet2/", "C217", 217.0).
?test(sheet2_D217, "/Sheet2/", "D217", 0.986363636363636).
?test(sheet2_A218, "/Sheet2/", "A218", " varpa/1,").
?test(sheet2_C218, "/Sheet2/", "C218", 218.0).
?test(sheet2_D218, "/Sheet2/", "D218", 0.990909090909091).
?test(sheet2_A219, "/Sheet2/", "A219", " weibull/4").
?test(sheet2_C219, "/Sheet2/", "C219", 219.0).
?test(sheet2_D219, "/Sheet2/", "D219", 0.995454545454545).
?test(sheet2_C220, "/Sheet2/", "C220", 220.0).
?test(sheet2_D220, "/Sheet2/", "D220", 1.0).
%% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

init_per_suite(Config) ->
    code:add_patha("../../../../../ebin"),
    production_boot:start(),
    bits:clear_db(),
    test_util:wait(),
    io:format("Current path:~n"),
    c:pwd(),
    Celldata = test_util:readxls("../../excel_files/Win_Excel07_As_97/" ++
                                 "b_rel_references.xls"),
    io:format("DATA:~n~p~n~n", [Celldata]),
    Postcell =
        fun({{{sheet, Sheetname}, {row_index, Row}, {col_index, Col}}, Val}) ->
                Postdata = conv_for_post(Val),
                Path = "/" ++ "b_rel_references" ++ "/" ++ Sheetname ++ "/",
                Ref = tconv:to_b26(Col + 1) ++ tconv:to_s(Row + 1),
                hnpost(Path, Ref, Postdata)
        end,
    foreach(Postcell, Celldata),
    Config.

end_per_suite(_Config) ->
    production_boot:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        sheet1_A1,
        sheet1_A2,
        sheet1_B2,
        sheet1_C2,
        sheet1_E2,
        sheet1_M2,
        sheet1_A3,
        sheet1_B3,
        sheet1_E3,
        sheet1_F3,
        sheet1_G3,
        sheet1_H3,
        sheet1_I3,
        sheet1_J3,
        sheet1_A4,
        sheet1_B4,
        sheet1_E4,
        sheet1_F4,
        sheet1_G4,
        sheet1_H4,
        sheet1_I4,
        sheet1_J4,
        sheet1_K4,
        sheet1_A5,
        sheet1_B5,
        sheet1_E5,
        sheet1_F5,
        sheet1_H5,
        sheet1_I5,
        sheet1_J5,
        sheet1_K5,
        sheet2_A1,
        sheet2_A2,
        sheet2_A3,
        sheet2_A4,
        sheet2_A5,
        sheet2_A6,
        sheet2_A7,
        sheet2_A8,
        sheet2_A9,
        sheet2_A10,
        sheet2_A11,
        sheet2_A12,
        sheet2_A13,
        sheet2_A14,
        sheet2_A15,
        sheet2_A16,
        sheet2_A17,
        sheet2_A18,
        sheet2_A19,
        sheet2_A20,
        sheet2_A21,
        sheet2_A22,
        sheet2_A23,
        sheet2_A24,
        sheet2_A25,
        sheet2_A26,
        sheet2_A27,
        sheet2_A28,
        sheet2_A29,
        sheet2_A30,
        sheet2_A31,
        sheet2_A32,
        sheet2_A33,
        sheet2_A34,
        sheet2_A35,
        sheet2_A36,
        sheet2_A37,
        sheet2_A38,
        sheet2_A39,
        sheet2_A40,
        sheet2_A41,
        sheet2_A42,
        sheet2_A43,
        sheet2_A44,
        sheet2_A45,
        sheet2_A46,
        sheet2_A47,
        sheet2_A48,
        sheet2_A49,
        sheet2_A50,
        sheet2_A51,
        sheet2_A52,
        sheet2_A53,
        sheet2_A54,
        sheet2_A55,
        sheet2_A56,
        sheet2_A57,
        sheet2_A58,
        sheet2_A59,
        sheet2_A60,
        sheet2_A61,
        sheet2_A62,
        sheet2_A63,
        sheet2_A64,
        sheet2_A65,
        sheet2_A66,
        sheet2_A67,
        sheet2_A68,
        sheet2_A69,
        sheet2_A70,
        sheet2_A71,
        sheet2_A72,
        sheet2_A73,
        sheet2_A74,
        sheet2_A75,
        sheet2_A76,
        sheet2_A77,
        sheet2_A78,
        sheet2_A79,
        sheet2_A80,
        sheet2_A81,
        sheet2_A82,
        sheet2_A83,
        sheet2_A84,
        sheet2_A85,
        sheet2_A86,
        sheet2_A87,
        sheet2_A88,
        sheet2_A89,
        sheet2_A90,
        sheet2_A91,
        sheet2_A92,
        sheet2_A93,
        sheet2_A94,
        sheet2_A95,
        sheet2_A96,
        sheet2_A97,
        sheet2_A98,
        sheet2_A99,
        sheet2_A100,
        sheet2_A101,
        sheet2_A102,
        sheet2_A103,
        sheet2_A104,
        sheet2_A105,
        sheet2_A106,
        sheet2_A107,
        sheet2_A108,
        sheet2_A109,
        sheet2_A110,
        sheet2_A111,
        sheet2_A112,
        sheet2_A113,
        sheet2_A114,
        sheet2_A115,
        sheet2_A116,
        sheet2_A117,
        sheet2_A118,
        sheet2_A119,
        sheet2_A120,
        sheet2_A121,
        sheet2_A122,
        sheet2_A123,
        sheet2_A124,
        sheet2_A125,
        sheet2_A126,
        sheet2_A127,
        sheet2_A128,
        sheet2_A129,
        sheet2_A130,
        sheet2_A131,
        sheet2_A132,
        sheet2_A133,
        sheet2_A134,
        sheet2_A135,
        sheet2_A136,
        sheet2_A137,
        sheet2_A138,
        sheet2_A139,
        sheet2_A140,
        sheet2_A141,
        sheet2_A142,
        sheet2_A143,
        sheet2_A144,
        sheet2_A145,
        sheet2_A146,
        sheet2_A147,
        sheet2_A148,
        sheet2_A149,
        sheet2_A150,
        sheet2_A151,
        sheet2_A152,
        sheet2_A153,
        sheet2_A154,
        sheet2_A155,
        sheet2_A156,
        sheet2_A157,
        sheet2_A158,
        sheet2_A159,
        sheet2_A160,
        sheet2_A161,
        sheet2_A162,
        sheet2_A163,
        sheet2_A164,
        sheet2_A165,
        sheet2_A166,
        sheet2_A167,
        sheet2_A168,
        sheet2_A169,
        sheet2_A170,
        sheet2_A171,
        sheet2_A172,
        sheet2_A173,
        sheet2_A174,
        sheet2_C174,
        sheet2_D174,
        sheet2_A175,
        sheet2_C175,
        sheet2_D175,
        sheet2_A176,
        sheet2_C176,
        sheet2_D176,
        sheet2_A177,
        sheet2_C177,
        sheet2_D177,
        sheet2_A178,
        sheet2_C178,
        sheet2_D178,
        sheet2_A179,
        sheet2_C179,
        sheet2_D179,
        sheet2_A180,
        sheet2_C180,
        sheet2_D180,
        sheet2_A181,
        sheet2_C181,
        sheet2_D181,
        sheet2_A182,
        sheet2_C182,
        sheet2_D182,
        sheet2_A183,
        sheet2_C183,
        sheet2_D183,
        sheet2_A184,
        sheet2_C184,
        sheet2_D184,
        sheet2_A185,
        sheet2_C185,
        sheet2_D185,
        sheet2_A186,
        sheet2_C186,
        sheet2_D186,
        sheet2_A187,
        sheet2_C187,
        sheet2_D187,
        sheet2_A188,
        sheet2_C188,
        sheet2_D188,
        sheet2_A189,
        sheet2_C189,
        sheet2_D189,
        sheet2_A190,
        sheet2_C190,
        sheet2_D190,
        sheet2_A191,
        sheet2_C191,
        sheet2_D191,
        sheet2_A192,
        sheet2_C192,
        sheet2_D192,
        sheet2_A193,
        sheet2_C193,
        sheet2_D193,
        sheet2_A194,
        sheet2_C194,
        sheet2_D194,
        sheet2_A195,
        sheet2_C195,
        sheet2_D195,
        sheet2_A196,
        sheet2_C196,
        sheet2_D196,
        sheet2_A197,
        sheet2_C197,
        sheet2_D197,
        sheet2_A198,
        sheet2_C198,
        sheet2_D198,
        sheet2_A199,
        sheet2_C199,
        sheet2_D199,
        sheet2_A200,
        sheet2_C200,
        sheet2_D200,
        sheet2_A201,
        sheet2_C201,
        sheet2_D201,
        sheet2_A202,
        sheet2_C202,
        sheet2_D202,
        sheet2_A203,
        sheet2_C203,
        sheet2_D203,
        sheet2_A204,
        sheet2_C204,
        sheet2_D204,
        sheet2_A205,
        sheet2_C205,
        sheet2_D205,
        sheet2_A206,
        sheet2_C206,
        sheet2_D206,
        sheet2_A207,
        sheet2_C207,
        sheet2_D207,
        sheet2_A208,
        sheet2_C208,
        sheet2_D208,
        sheet2_A209,
        sheet2_C209,
        sheet2_D209,
        sheet2_A210,
        sheet2_C210,
        sheet2_D210,
        sheet2_A211,
        sheet2_C211,
        sheet2_D211,
        sheet2_A212,
        sheet2_C212,
        sheet2_D212,
        sheet2_A213,
        sheet2_C213,
        sheet2_D213,
        sheet2_A214,
        sheet2_C214,
        sheet2_D214,
        sheet2_A215,
        sheet2_C215,
        sheet2_D215,
        sheet2_A216,
        sheet2_C216,
        sheet2_D216,
        sheet2_A217,
        sheet2_C217,
        sheet2_D217,
        sheet2_A218,
        sheet2_C218,
        sheet2_D218,
        sheet2_A219,
        sheet2_C219,
        sheet2_D219,
        sheet2_C220,
        sheet2_D220
    ].

-define(HNSERVER, "http://127.0.0.1:9000").

hnget(Path, Ref) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    {ok, {{_V, Code, _R}, _H, Body}} = http:request(get, {Url, []}, [], []),
    io:format("Code for ~p~p is ~p.~nBody is: ~p~n~n", [Path, Ref, Code, Body]),
    Body.
  
hnpost(Path, Ref, Postdata) ->
    Url = ?HNSERVER ++ Path ++ Ref,
    Postreq = "<create><formula>" ++ Postdata ++ "</formula></create>",
    Return = http:request(post,
                          {Url, [], "text/xml", Postreq},
                          [], []),
    {ok, {{_V, Code, _R}, _H, Body}} = Return,
    io:format("Posted ~p to ~p~p.~nResponse code: ~p. Response body: ~p.~n~n", 
              [Postdata, Path, Ref, Code, Body]),
    Return.

cmp(G, E) ->
    Val = conv_from_get(G),
    Val == E.

conv_from_get(Val) ->
    case Val of
        [34 | Tl] -> % String
            hslists:init(Tl);
        [39 | Tl] -> % Atom, i.e. an error value.
            list_to_atom(hslists:init(Tl));
        "TRUE" ->
            true;
        "FALSE" ->
            false;
        _ ->
            tconv:to_num(Val)
    end.

conv_for_post(Val) ->
    case Val of
        {_, boolean, true} -> "true";
        {_, boolean, fase} -> "false";
        {_, number, N}     -> tconv:to_s(N);
        {_, error, E}      -> E;
        {string, X}        -> "\"" ++ X ++ "\"";
        {formula, F}       -> F
    end.

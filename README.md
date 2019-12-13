mgsgbLz
=========
GB Metal Gear Solid: Ghost Babel script tool. Game uses MTE and LZ compression for text. This tool can work with it.


Synopsis:
```
mgsgbLz -r <input>                      
mgsgbLz -ra <inputs> <weights>
mgsgbLz -d -b <input> <offs>     
mgsgbLz -d -s <input>            
mgsgbLz -c -i <inst> <fst> <ptr> 
mgsgbLz -c -b <input>            
mgsgbLz -t <input>               
Options:
-h     
-v     
```
  
Description

***-r <input> ***               Rebuild dictionary from plain script file.

***-ra <inputs> <weights>***   Rebuild adaptive from multipe files, specifying weight of this file (what part of dictionary will be devoted to this script file). For instance, mgsgblz -ra "instance #00.txt" 1 "instance #03.txt" 1 "instance #08.txt" 3 "instance #09.txt" 2 will build dictionary mostly from "instance #08.txt", compressing it with MTE at a higher rates.

***-d -b <input> <offs>***     Decompress one LZ block from input ROM at offset.\n\

***-d -s <input>***            Decode full script from input gameboy ROM.

***-c -i <inst> <fst> <ptr>*** Encode one inst with given fst message and ptr. Use for script translation.

***-c -b <input>***            Compress one input plain binary block with LZ. Useful for tiles/nametables binary compression for further ROM insertion.

***-t <input>***               Test check input plain script for various issues. In almost all cases you don't want line length to be more than 24 symbols. In normal game script (everywhere, except Instance #00), there shouldn't be more than two lines with [nl] symbols. You should use [clr] or [scrl] for next newline. All valid symbols are mentioned in table.tbl. -t command will warn you in case you violate any of those rules. Also during compression, tool will warn you if your instance will not fit WRAM or appropriate ROM space.

Options:

***-h***     Show help.

***-v***     Show version.



See additional files in [release](https://github.com/romhack/mgsgbLz/releases/latest) archive. Usage examples are in compress.bat file. Recommended translation scheme:  

1. translate script instance  

2. mgsgblz -t "instance #XX.txt" (checking and correcting possible errors)
  
3. buildMteDictionary.bat (rebuilding table from translated instances)   

4. compress.bat XX (for comperssion and insertion newly translated instance)
  
5. check translation
  
6. go to 4


Build:
```
stack setup
stack build
```
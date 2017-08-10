mgsgbLz
=========
GB Metal Gear Solid: Ghost Babel script tool. Game uses MTE and LZ compression for text. This tool can work with it.


Synopsis:
```
mgsgbLz -m                       
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

***-m***                       Rebuild MTE dictionary from plain script (SLOW!). Script should be UTF8 (no BOM) encoded. 

***-d -b <input> <offs>***     Decompress one LZ block from input ROM at offset. 

***-d -s <input>***            Decode full script from input gameboy ROM.

***-c -i <inst> <fst> <ptr>*** Encode one instance with given fst message and ptr. Just to quickly check translation

***mgsgbLz -c -b <input>***            Compress one input plain binary block with LZ. Useful for tiles/nametables binary compression for further ROM insertion.

***mgsgbLz -t <input>***               Test check plain script for various issues. In almost all cases you don't want line length to be more than 24 symbols. In normal game script (everywhere, except Instance #00), there shouldn't be more than two lines with ¶ symbols. You should use ¤ or ⌂ for next newline. All valid symbols are mentioned in Table.hs. -t command will warn you in case you violate any of those rules. Also during compression, tool will warn you if your instance will not fit WRAM or appropriate ROM space.

Options:

***-h***     Show help.

***-v***     Show version.



See additional files in [release](https://github.com/romhack/mgsgbLz/releases/latest) archive. Usage examples are in patchScriptInstance.bat file. Recommended translation scheme:  

1. translate script instance  

2. patchScriptInstance.bat check
  
2. patchScriptInstance.bat tblCompAll   
  
3. check translation
  
4. patchScriptInstance.bat X, where X is your current instance
  
5. go to 3


Build:
```
stack setup
stack build
```
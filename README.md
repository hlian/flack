~~~
            ~ walls ~


        _____,    _..-=-=-=-=-====--,
     _.'a   /  .-',___,..=--=--==-'`
    ( _     \ /  //___/-=---=----'
     ` `\    /  //---/--==----=-'
  ,-.    | / \_//-_.'==-==---='
 (.-.`\  | |'../-'=-=-=-=--'
  (' `\`\| //_|-\.`;-~````~,        _
       \ | \_,_,_\.'        \     .'_`\
        `\            ,    , \    || `\\
          \    /   _.--\    \ '._.'/  / |
          /  /`---'   \ \   |`'---'   \/
         / /'          \ ;-. \
      __/ /           __) \ ) `|   jgs
    ((='--;)         (,___/(,_/


       a social network from
          the early 2000s



## ingredients

• a working npm installation
  (`brew install npm`)
• a working stack installation
  (https://github.com/commercialhaskell/stack/releases)
• cd to web/
  and run `npm install`

## development

• in one tab,
  cd to web/
  and run `npm start`
• in a second tab,
  cd to server/
  and run `stack build --fast --file-watch --exec walls`
• in a third tab,
  cd to here
  and run `nginx -c nginx.conf -p $(pwd)`

~~~

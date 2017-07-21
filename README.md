<pre>


            ~ <b>flack</b> ~
         a slack concern


## <b>ingredients</b>

  • <i>brew install yarn</i>

  • <a href="https://github.com/commercialhaskell/stack/releases">a working stack installation</a>

  • cd to web/
     • <i>yarn</i>

  • [NEW & DIFFERENT]
    copy etc/env.sh to env.sh
    and fill in details

## <b>development</b>

  • in one tab,
    cd to web/
    and run <i>yarn webpack -- --watch</i>

  • in a second tab,
    cd to server/
    • run <i>source ../env.sh</i>
    • run <i>stack build --fast --file-watch --exec walls</i>

  • in a third tab,
    cd to here
    and run <i>nginx -c nginx.conf -p $(pwd)</i>

  • in a browser, go to
    http://localhost:3000/


</pre>

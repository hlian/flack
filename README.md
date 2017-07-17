~~~
            ~ flack ~
          a slack concern

## ingredients

• `brew install yarn watchify`
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
• in a browser, go to
  http://localhost:3000/

~~~

# use /etc/hosts to make this happen
export flackaddr="flack.local"
export flackport="80"
export flackpath="/"
export flackclientid="this is the client ID you get from registering a Slack app and then configuring Permissions"
export flackclientsecret="this is the client secret you get from registering a Slack app and then configuring Permissions"
export flackteam="this is the slack team you want to jazz up"

env | grep "^flack"

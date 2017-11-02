
<?php

function http_digest_parse($$txt)
{
  // protect against missing data
  $$needed_parts = array('nonce'=>1, 'nc'=>1, 'cnonce'=>1, 'qop'=>1, 'username'=>1, 'uri'=>1, 'response'=>1);
  $$data = array();
  $$keys = implode('|', array_keys($$needed_parts));

  preg_match_all('@(' . $$keys . ')=(?:([\'"])([^\2]+?)\2|([^\s,]+))@', $$txt, $$matches, PREG_SET_ORDER);
  foreach ($$matches as $$m) {
    $$data[$$m[1]] = $$m[3] ? $$m[3] : $$m[4];
    unset($$needed_parts[$$m[1]]);
  }
  return $$needed_parts ? false : $$data;
}

# TODO
function get_group_from_url()
{ 
  $$string = explode('/', $$_SERVER['REQUEST_URI'])[2];
    $$url = sprintf( "%03d", (int) $$string );
    return $$url;
}

function check_group($$group,$$username)
{
  if(strlen($$username) != 13)
  { die('Wrong user name'); }
  $$user_group = substr($$username,8,3);
  return strcmp($$group,$$user_group);
}

function load_users()
{
  $$file_contents = explode("\n", file_get_contents("$logindb$")); 
  $$users = array();    
  foreach($$file_contents as $$user)
  { $$tmp = explode(":", $$user);
    $$users[$$tmp[0]] = $$tmp[1];
  }
  return $$users;
}

  $$realm = get_group_from_url();

  # TODO check env var _SERVER
  if(empty($$_SERVER['PHP_AUTH_DIGEST']))
  {
    header('HTTP/1.1 401 Unauthorized');
    header('WWW-Authenticate: Digest realm="'.$$realm.'",qop="auth",nonce="'.
            uniqid().'",opaque="'.md5($$realm).'"');
    die('Please Login, group ' . get_group_from_url() . '');
  }

  $$users = load_users();

  if(!($$data = http_digest_parse($$_SERVER['PHP_AUTH_DIGEST'])) ||
     !isset($$users[$$data['username']]) || check_group($$realm, $$data['username']) != 0)
  { 
    header('HTTP/1.1 401 Unauthorized');
    header('WWW-Authenticate: Digest realm="'.$$realm.'",qop="auth",nonce="'.uniqid().'",opaque="'.md5($$realm).'"');
    die('Please Login.');
  }

  // generate the valid response
  $$A1 = md5($$data['username'] . ':' . $$realm . ':' . $$users[$$data['username']]);
  $$A2 = md5($$_SERVER['REQUEST_METHOD'].':'.$$data['uri']);
  $$valid_response = md5($$A1.':'.$$data['nonce'].':'.$$data['nc'].':'.$$data['cnonce'].':'.$$data['qop'].':'.$$A2);

  if($$data['response'] != $$valid_response)
  {
    header('HTTP/1.1 401 Unauthorized');
    header('WWW-Authenticate: Digest realm="'.$$realm.'",qop="auth",nonce="'.uniqid().'",opaque="'.md5($$realm).'"');
    die('Please Login.');
  }

  // TODO : remove ok, valid username & password
  echo 'You are logged in as: ' . $$data['username'];
?>
$code$


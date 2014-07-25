<?php
namespace geeklog;
use \stdClass;
use Ratamarkup;

$settings = array(
		  'data_path' => '/site/www/geekmx/data',
		  'name_re'   => '/^([0-9a-z_-]*).*$/',
		  '404'       => '404',
		  'suffixes'  => array('.txt','.html'),
		  'transform' => 'ratamarkup',
		  );

$meta_keys = array('metatime', 'title', 'author', 'tags', 'keywords', 'description', 'timestamp' );

@include_once("/site/scripts/ratamarkup/ratamarkup.php");
exit( main() );

function main() {

  check_xattr();

  header('Content-type: text/html; charset=utf-8');
  $doc = parse( $_REQUEST['doc'] );
  include_once("templates/geekmx.php");

  return 0;

}

function check_xattr() {

  global $settings, $xattr;

  if ( extension_loaded('xattr') ) {
    $xattr_ext = new \ReflectionExtension('xattr');
    $xattr = $xattr_ext->getFunctions();
  }

  if ( $xattr['xattr_supported']->invoke( $settings['data_path'] ) ) {
    $settings['use_xattr'] = true;
  }

}

function clean_name($name) {
  $name = preg_replace(array('/á/u','/é/u','/í/u','/ó/u','/ú/u','/ñ/u','/ü/u'),array('a','e','i','o','u','n','u'), $name);
  $name = preg_replace('/^([0-9a-z_-]*).*$/','$1',$name);
  return $name != '' ? $name : null;
}

function parse($name,$filename = null,$meta = false) {

  global $settings;

  $doc = new stdClass();

  if ( $filename === null ) {

    $file = clean_name($name);

    if ( $file === null ) {
      $doc->body = "<!-- invalid file: $name -->\n";
      return $doc;
    }

    $filename = "$settings[data_path]/$file";

    if ( ! file_exists( $filename ) ) {

      foreach ( $settings['suffixes'] as $suffix ) {
	if ( file_exists( $filename.$suffix ) ) {
	  $filename .= $suffix;
	  break;
	}
      }

      if ( ! file_exists( $filename ) ) {
	// not found
	if ($meta) return null;
	$doc->body = "<!-- file not found $filename -->\n";
	return $doc;
      }

    }

  }

  $doc->filename = $filename;
  $doc->mtime = filemtime($filename);

  if ( $meta && $settings['use_xattr'] ) {
    // try to load metadata from xattrs
    $xattrs = load_xattrs($filename);
    if ( $xattrs['metatime'] >= filemtime($filename) ) {
      // data is not stale
      foreach ( $xattrs as $key => $value ) {
	$doc->{$key} = $value;
      }
      return $doc;
    }
  }

  $contents = file_get_contents($filename);

  list($doc->headers, $doc->body) = explode("\n\n", $contents, 2);

  foreach ( explode("\n", $doc->headers) as $line ) {
    list($key, $value) = preg_split('/\s*:\s*/', $line, 2);

    switch ( $key ) {
    case '' : continue; break;
    case 'tags': $value = explode(' ', $value); break;
    case 'timestamp': 
      if ( !is_numeric($value) ) {
	$value = strtotime($value);
      }
      break;
    }

    $doc->$key = $value;
  }

  if ( $meta ) {
    $doc->body = '';
    if ( $settings['use_xattr'] ) save_xattrs($filename, $doc);
  }
  else {
    $transform = $doc->transform ? $doc->transform : $settings['transform'];
    if ( function_exists( "geeklog\\transform_" . $transform ) ) {
      $doc->body = call_user_func( "geeklog\\transform_" . $transform, $doc->body );
    }
  }

  return $doc;

}

function load_xattrs($filename) {

  global $meta_keys, $xattr;
  $attrs = array();

  foreach ( $meta_keys as $key ) {
    $attrs[$key] = $xattr['xattr_get']->invoke($filename, 'geeklog.'.$key);
    switch ( $key ) {
    case 'tags':
      $attrs[$key] = explode(' ', $attrs[$key]);
      break;
    }
  }

  return $attrs;
}

function save_xattrs($filename,$doc) {

  global $meta_keys, $xattr;
  foreach ( $meta_keys as $key ) {
    $value = $doc->{$key};
    if ( $key == 'metatime' ) $value = time();
    if ( $key == 'tags' && is_array($value)  ) $value = implode(' ', $value);
    $xattr['xattr_set']->invoke($filename, 'geeklog.'.$key, $value );
  }

}

function html_pclean($text) {
  $text = html_clean($text);
  $text = str_replace('"','&quot;',$text);
  return $text;
}

function html_clean($text) {
  $text = str_replace('&','&amp;',$text);
  $text = str_replace('<','&lt;',$text);
  $text = str_replace('>','&gt;',$text);
  return $text;
}

function transform_ratamarkup($text) {
  global $settings;

  $link_callback = function($m) {

    global $settings;

    list($str,$href,$text) = $m;

    if ( !$text ) {
      $text = $href;
      $href = str_replace(" ", "_", strtolower($href));
    }

    if ( $href == clean_name($href) ) {
      $ref = parse($href,null,true);

      if ( $ref === null ) {
	return "<span class=\"notfound\">$text</span>";
      }

      $items = array();
      if ( $ref->description ) array_push($items, 'title="'.html_pclean($ref->description).'"');

      return "<a href=\"$href\"". implode(" ", $items) .">$text</a>";
    }

    return "<a href=\"$href\" rel=\"nofollow\">$text</a>";
  };

  return Ratamarkup\process( $text, null, array( 'link_callback' => $link_callback ) );
}

function transform_ratamarkup_cli($text) {

  $desc = array( 0 => array('pipe','r'), 1 => array('pipe','w'), 2 => array('pipe', 'w') );
  $cmd = "/site/scripts/ratamarkup/ratamarkup";

  $p = proc_open($cmd, $desc, $pipes);

  fwrite( $pipes[0], $text );
  fclose( $pipes[0] );

  $processed = stream_get_contents( $pipes[1] );

  fclose( $pipes[1] );
  fclose( $pipes[2] );
  proc_close($p);

  return $processed;

}

function transform_php($text) {
  return eval($text);
}

function transform_ratamarkup_php($text) {
  $mk = eval($text);
  return transform_ratamarkup($mk);
}

function list_files() {

  global $settings;

  $docs = array();

  $filenames = scandir($settings['data_path']);
  foreach ( $filenames as $filename ) {
    if ( $filename == clean_name($filename) ) {
      $doc = parse($filename,null,true);
      array_push( $docs, $doc );
      continue;
    }
    foreach ( $settings['suffixes'] as $suffix ) {
      if ( $filename == clean_name($filename) . $suffix ) {
	$doc = parse($filename,null,true);
	array_push( $docs, $doc );
	continue;
      }
    }
  }

  return $docs;

}

function search($params = array()) {

  $docs = list_files();
  $results = array();

  foreach ( $params as $key => $value ) {

    foreach ( $docs as $doc ) {

      switch ($key) {
      case 'has_timestamp':
	if ( !is_numeric($doc->timestamp) )
	  continue 2;
	break;

      case 'timestamp_before':
	if ( !is_numeric($doc->timestamp) || (int)($doc->timestamp) == 0 || (int)($doc->timestamp) > (int)$value )
	  continue 2;
	break;

      case 'timestamp_before_now':
	if ( !$value ) break;
	if ( !is_numeric($doc->timestamp) || (int)($doc->timestamp) == 0 || (int)($doc->timestamp) > time() )
	  continue 2;
	break;

      case 'timestamp_after':
	if ( !is_numeric($doc->timestamp) || (int)($doc->timestamp) == 0 || (int)($doc->timestamp) < (int)$value )
	  continue 2;
	break;

      case 'tag':
	if ( !is_array($doc->tags) || !in_array( $value, $doc->tags ) )
	  continue 2;
	break;

      case 'not_tag':
	if ( is_array($doc->tags) && in_array( $value, $doc->tags ) )
	  continue 2;
	break;

      default:
	break;

      }

      array_push($results, $doc);
    }

    $docs = $results;
    $results = array();

  }

  if ( $params['sort'] != '' ) {
    $will_sort = array();
    foreach ( $docs as $doc ) {
      switch ( $params['sort'] ) {
      case 'mtime':
      case 'time':
	$will_sort[ $doc->mtime . "|" . strtolower( $doc->title ) ] = $doc;
	break;
      case 'timestamp':
	$will_sort[ $doc->timestamp . "|" . strtolower( $doc->title ) ] = $doc;
	break;
      case 'timestamp':
      default:
	array_push($will_sort,$doc);
      }
    }

    if ( $params['reverse'] )
      krsort($will_sort);
    else
      ksort($will_sort);

    return $will_sort;
  }

  return $results;

}

namespace Ratamarkup;

function block_omg($acc,$tokens) {
  return "<p>OMG</p>\n";
}

function block_blog($acc,$tokens) {

  $params = array();

  foreach ( explode("\n",$acc) as $token ) {
    list($k,$v) = preg_split('/\s*=\s*/', $token, 2);
    if ( $k == '' ) continue;
    $params[$k] = $v;
  }

  $list = \geeklog\search($params);

  foreach ( $list as $file ) {
    $doc = \geeklog\parse($file->filename,$file->filename);
    list($top,$rest) = explode("<!-- break -->", $doc->body);
    $top = preg_replace('/(<\\/)?h1>/', '$1h2>', $top);
    $out .= $top;
    $link = "<a href=\"".\geeklog\clean_name(basename($file->filename)).
      "\" title=\"".$file->title."\">";
    $date = date( DATE_RFC822, (int)($file->timestamp) );
    if ( $rest ) {
      $out .= "<p>$link"."Leer el resto</a></p>\n";
    }
    $out .= "<p class=\"blog_footer\"><i>{$date} por $doc->author</i> [{$link}Permalink</a>]</p>\n";
  }

  return $out;
}

function block_doclist($acc,$tokens) {

  $params = array();

  foreach ( explode("\n",$acc) as $token ) {
    list($k,$v) = preg_split('/\s*=\s*/', $token, 2);
    if ( $k == '' ) continue;
    $params[$k] = $v;
  }

  $list = \geeklog\search($params);

  $out = "<ul>\n";

  foreach ( $list as $file ) {
    $doc = \geeklog\parse($file->filename,$file->filename);

    $link = "<a href=\"".\geeklog\clean_name(basename($file->filename)).
      "\" title=\"".$file->description."\">";

    $date = date( DATE_RFC822, (int)($file->timestamp) );
    $mtime = date( DATE_RFC822, (int)($file->mtime) );

    $out .= "<li><b>{$link}$file->title</a></b> ($mtime | $date)</li>\n";

  }

  $out .= "</ul>";

  return $out;
}

function block_break($acc,$tokens) {
  return "<!-- break -->\n";
}

function block_include($acc,$tokens) {

  global $included_already;

  $out = '';

  array_shift($tokens);

  foreach ( $tokens as $token ) {
    $doc = \geeklog\parse($token);
    if ( !$doc->body ) continue;
    $out .= "$doc->body\n";
  }
  return $out;
}


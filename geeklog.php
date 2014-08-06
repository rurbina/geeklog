<?php
namespace geeklog;
use \stdClass;
use Ratamarkup;

$settings = array(
		  'data_path'        => '/site/www/geekmx/data',
		  'name_re'          => '/^([0-9a-z_-]*).*$/',
		  '404'              => '404',
		  'suffixes'         => array('.txt','.html'),
		  'transform'        => 'ratamarkup',
		  'template'         => '/site/www/geekmx/templates/geekmx.php',
		  'date_time_format' => '%c',
		  'date_format'      => '%F',
		  'time_format'      => '%k:%M',
		  );

@include_once("geeklog.config.php");

$meta_keys = array(
		   'metatime', 
		   'title',
		   'author',
		   'tags',
		   'keywords',
		   'description',
		   'timestamp',
		   'comment',
		   'revision',
		   );

require_once("/site/scripts/ratamarkup/ratamarkup.php");

exit( main() );

function main() {

  global $settings;

  check_xattr();

  header('Content-type: text/html; charset=utf-8');
  $doc = parse( $_REQUEST['doc'] );

  require_once($settings['template']);

  return 0;

}

function check_xattr() {

  global $settings, $xattr;

  if ( extension_loaded('xattr') ) {
    $xattr_ext = new \ReflectionExtension('xattr');
    $xattr = $xattr_ext->getFunctions();
  }

  if ( @$xattr['xattr_supported']->invoke( $settings['data_path'] ) ) {
    $settings['use_xattr'] = true;
  }

}

function clean_accents($str) {
  return preg_replace(array('/á/iu','/é/iu','/í/iu','/ó/iu','/ú/iu','/ñ/iu','/ü/iu'),array('a','e','i','o','u','n','u'), $str);
}

function clean_name($name) {
  $name = clean_accents($name);
  $name = preg_replace('/^([0-9a-z_-]*).*$/','$1',$name);
  return $name != '' ? $name : null;
}

function add_pretty_print_items(&$doc) {

  global $settings;

  $doc->name = clean_name( preg_replace('/\\..*$/', '', basename($doc->filename) ) );

  if ( !$doc->title ) $doc->title = $doc->name;

  $a_title = html_pclean( $doc->description ? $doc->description : $doc->title );
  $doc->title_link = "<a href=\"{$doc->name}\" title=\"$a_title\">".html_clean($doc->title)."</a>";

  $doc->name_link = "<a href=\"{$doc->name}\" title=\"$a_title\">".html_clean($doc->name)."</a>";

  $doc->tags_string = is_array($doc->tags) ? implode(', ', $doc->tags) : '';

  $doc->mtime_format = strftime($settings['mtime_format'] ? 
				$settings['mtime_format'] : 
				$settings['date_time_format'], 
				$doc->mtime);

  $doc->timestamp_format = strftime($settings['timestamp_format'] ? 
				    $settings['timestamp_format'] : 
				    $settings['date_time_format'],
				    strtotime($doc->timestamp) );

  $timestamp = is_numeric($doc->timestamp) ? $doc->timestamp : strtotime($doc->timestamp);

  $doc->timestamp_date_format = $timestamp ?
    strftime($settings['timestamp_date_format'] ? 
	     $settings['timestamp_date_format'] : 
	     $settings['date_format'],
	     $timestamp ) : '';
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
      add_pretty_print_items($doc);
      return $doc;
    }
  }

  $contents = file_get_contents($filename);

  list($doc->headers, $doc->body) = explode("\n\n", $contents, 2);

  foreach ( explode("\n", $doc->headers) as $line ) {
    list($key, $value) = preg_split('/\s*:\s*/', $line, 2);

    switch ( $key ) {
    case '' : continue 2; break;
    case 'tags': $value = explode(' ', $value); break;
    case 'timestamp': 
      if ( !is_numeric($value) ) {
	$value = strtotime($value);
      }
      break;
    }

    $doc->$key = $value;
  }

  // if we only want metadata then discard the contents
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

  add_pretty_print_items($doc);
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
      $href = clean_accents($href);
      $href = str_replace(" ", "_", strtolower($href));
    }

    if ( $href == clean_name($href) ) {
      $ref = parse($href,null,true);

      if ( $ref === null ) {
	return "<span class=\"notfound\" href=\"$href\">$text</span>";
      }

      $items = array();
      if ( $ref->description ) array_push($items, 'title="'.html_pclean($ref->description).'"');

      return "<a href=\"$href\"". implode(" ", $items) .">$text</a>";
    }

    return "<a href=\"$href\" rel=\"nofollow\">$text</a>";
  };

  return Ratamarkup\process( $text, null, array( 'link_callback' => $link_callback ) );
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
	// this is a special case of or_tags containing only one
      case 'or_tags':
	// docs containing any of these will match
	if ( !is_array($doc->tags) ) continue 2;
	$tags = preg_split('/\s*,\s*/',$value);

	foreach ( $tags as $tag ) { 
	  if ( in_array( $tag, $doc->tags ) ) {
	    break 2;
	  }
	}
	continue 2;

      case 'and_tags':
	// docs containing all of these will match
	if ( !is_array($doc->tags) ) continue 2;
	$tags = preg_split('/\s*,\s*/',$value);

	foreach ( $tags as $tag ) { 
	  if ( !in_array( $tag, $doc->tags ) ) {
	    continue 3;
	  }
	}
	break;

      case 'not_tag':
	if ( !is_array($doc->tags) ) break;
	if ( !in_array( $value, $doc->tags ) ) break;
	continue 2;

      case 'match_all':
	break;

      default:
	break;

      }

      array_push($results, $doc);
    }

    $docs = $results;
    $results = array();

  }

  if ( $params['sort'] ) {
    $will_sort = array();
    $sort_flag = SORT_REGULAR;

    foreach ( $docs as $doc ) {
      switch ( $params['sort'] ) {
      case 'mtime':
      case 'time':
	$will_sort[ $doc->mtime . " |" . strtolower( $doc->title ) ] = $doc;
	break;
      case 'timestamp':
	$will_sort[$doc->timestamp] = $doc;
	$sort_flag = SORT_NUMERIC;
	break;
      default:
	array_push($will_sort,$doc);
      }
    }

    if ( $params['reverse'] )
      krsort($will_sort, $sort_flag);
    else
      ksort($will_sort, $sort_flag);

    return $will_sort;
  }

  return $docs;

}

////////////////////////////////////////////////////////////
// These functions extend Ratamarkup

namespace Ratamarkup;

/*
  block_blog - lists articles
  - articles can (should?) be filtered by tags
  - they can (should?) be sorted by date descending
  - if they include a break (a line with !break in ratamarkup or <!-- break --> in html)
    then it stops there
  - it adds a date in customizable pretty-format (set via the 'timestamp' header)
  - adds an author ('author' header)
  - adds a permalink

  In the future it will also include sharing links for Facebook, Twitter and whatnot
*/
function block_blog($acc,$tokens) {

  global $settings;

  if ( !$settings['date_format'] )
    $settings['date_format'] = '%c';

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
    $date = strftime( $settings['date_format'], (int)($file->timestamp) );
    if ( $rest ) {
      $out .= "<p class=\"blog_break\">$link"."Leer el resto</a></p>\n";
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

function block_doclist_table($acc,$tokens) {

  global $settings;
  $strftime_format = $settings['date_format'] ? $settings['date_format'] : '%c';

  $params = parse_tokens_as_config($tokens);

  $list = \geeklog\search($params);

  $fields = preg_split('/\s*,\s*/', $params['fields']);
  $headers = preg_split('/\s*,\s*/', $params['headers'] ? $params['headers'] : $params['fields'] );
  $widths = preg_split('/\s*,\s*/', $params['widths']);

  if ( !is_array($widths) ) $widths = array();

  $out = "<table>\n\t<tr>\n";

  foreach ( $headers as $h ) {
    $w = array_shift($widths);
    if ( $w ) $w = " width=\"$w\"";

    $out .= "\t\t<th$w>$h</th>\n";
  }

  $out .= "\t</tr>\n";

  $list = \geeklog\search($params);

  foreach ( $list as $file ) {

    $doc = \geeklog\parse($file->filename,$file->filename,true);

    $out .= "\t<tr>\n";
    foreach ( $fields as $f ) {
      $value = $f ? $doc->$f : '';
      $out .= "\t\t<td>$value</td>\n";
    }
    $out .= "\t</tr>\n";

  }

  $out .= "</table>\n";

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

function block_lyrics($acc, $tokens, $opt) {

  $lines = explode("\n", $acc);
  array_walk($lines, function (&$v,$k) {
      $v = character_normal($v);
    });

  $out = '<div class="lyrics">' . implode("<br>\n", $lines) . '</div>';

  return $out;

}

function block_orgtbl($acc, $tokens, $opt) {

  $settings = parse_tokens_as_config($tokens);

  $lines = explode("\n",$acc);

  $rows = array();

  $rowspan = array();

  $col_widths = preg_split('/\s*,\s*/', $settings['widths']);

  foreach ( $lines as $line ) {
    $cells = explode( "|", $line );

    // clear exceeding stuff
    array_shift($cells);
    array_pop($cells);

    $row = array( 'cells' => $cells );

    if ( !$settings['rowspan'] ) {
      if ( preg_match('/^\|-/', $line) ) continue;

      array_walk($row['cells'], function(&$v,$k) {
	  $v = str_replace('\vert','|',$v); 
	  $v = character_normal($v);
	});

      array_push($rows, $row);

    }
    // opted-in to rowspans, so you need divisions
    else {

      if ( preg_match('/^\|-/', $line) ) {

	if ( sizeof($rowspan[0]['cells']) == 0 ) {
	  $rowspan = array();
	  continue;
	}

	$merged = array();
	foreach ( $rowspan as $row ) {
	  foreach ( $row['cells'] as $idx => $v ) {
	    $merged[$idx] .= "$v\n";
	  }
	}

	array_walk($merged, function(&$v,$k, $o) {
	    $v = str_replace('\vert','|',$v); 
	    $v = preg_replace('/^\s+|\s+$/m','',$v);
	    $v = block_normal($v, array(), $o);
	  },
	  $opt
	  );

	$row['cells'] = $merged;

	array_push($rows, $row);

	$rowspan = array();
      }
      else {
	array_push($rowspan, $row);
      }
    }

  }

  $rendered = "<table><tbody>\n";
  $header = true;

  foreach ( $rows as $row ) {

    if ( !is_array($row['cells']) ) continue;

    $rendered .= "\t<tr>\n";

    foreach ( $row['cells'] as $cell ) {
      if ( $header ) {
	$width = array_shift($col_widths);
	$tag = $width ? "<th width=\"$width\">" : "</th>";
	$rendered .= "\t\t$tag$cell</th>\n";
      }
      else {
	$rendered .= "\t\t<td>$cell</td>\n";
      }
    }

    $rendered .= "\t</tr>\n";
    $header = false;

  }

  $rendered .= "</tbody></table>\n";

  return $rendered;

}

function block_toc($acc,$tokens,$opt) {
}

function block_soundcloud_player($acc, $tokens, $opt) {

  $params = array();
  $defaults = array(
		    'width' => '100%',
		    'height' => '450',
		    'style' => 'float:right;width:45%',
		    );

  foreach ( $tokens as $token ) {
    list($k,$v) = explode("=", $token, 2);
    if ( preg_match('/^§/u',$k) ) continue;

    $params[$k] = $v;
  }

  foreach ( $defaults as $k => $v ) {
    if ( $params[$k] == "" ) $params[$k] = $v;
  }

  array_walk($params, function(&$v,$k) { $v = \geeklog\html_pclean($v); } );

  if ( $params['playlist'] != "" ) {
    $params['src'] = "https://w.soundcloud.com/player/?url=https%3A//api.soundcloud.com/playlists/$params[playlist]".
      "&amp;auto_play=false&amp;hide_related=false&amp;show_comments=true&amp;show_user=true&amp;show_reposts=false&amp;visual=false";
  }

  $iframe = "<iframe class=\"soundcloud_player $params[class]\" width=\"$params[width]\" height=\"$params[height]\" scrolling=\"no\" frameborder=\"no\" src=\"$params[src]\" style=\"$params[style]\"></iframe>\n";

  return $iframe;

}


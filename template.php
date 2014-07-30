<!DOCTYPE html>
<html>
<head>
<title><?= $doc->title ?> :: GeekMX</title>
<?php if ( $doc->keywords ) : ?>
<meta name="keywords" content="<?= $doc->keywords ?>">
<?php endif ?>
<?php if ( $doc->description ) : ?>
<meta name="description" content="<?= $doc->description ?>">
<?php endif ?>
<link rel="stylesheet" href="/geekmx.css">
</head>
<body>
<h1>GeekMX</h1>
<?= $doc->body ?>
</body>
</html>

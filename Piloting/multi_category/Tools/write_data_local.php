<?php
$data = $_POST['data']; // Get the data from the POST request
$filename = $_POST['filename']; // Get the desired filename from the POST request
//$filepath = $_POST['filepath'].$filename; // Set the filepath to your desired folder and filename
$file = fopen($filename, "w");
fwrite($file, $data);
fclose($file);
echo "File saved successfully.";
?>

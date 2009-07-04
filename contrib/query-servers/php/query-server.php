#!/usr/bin/env php
<?php
error_reporting(E_ALL);
include("Log.php");
include("Dispatcher.php");
include("Error.php");
include("ViewHandler.php");
include("utils.php");

define("DEBUG", true);
define("DEBUG_LOGFILE", "qs.php.log");

while($line = readline("")) {
    $jline = json_decode($line);
    $cmd = array_shift($jline);
    Log::line("$cmd start");
    Dispatcher::run($cmd, $jline);
    Log::line("$cmd done");
    flush();
}


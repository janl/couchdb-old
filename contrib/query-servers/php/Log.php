<?php
class Log
{
    static function line($line)
    {
        if(!DEBUG) {
            return;
        }

        static $fp = false;

        if(!$fp) {
            $fp = fopen(DEBUG_LOGFILE, "a") 
                or die("Unable to open logfile '$logfile'.");
        }
        $time = time();
        $line = var_export($line, true);
        fwrite($fp, "($time) $line\n");
    }
}

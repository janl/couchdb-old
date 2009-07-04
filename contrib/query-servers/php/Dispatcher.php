<?php
// Dispatcher.php
class Dispatcher
{
    static function _get_commands()
    {
        return array(
            "reset" => array("ViewHandler", "reset"),
            "add_fun" => array("ViewHandler", "add_fun"),
            "map_doc" => array("ViewHandler", "map_doc")
            );
    }

    static function run($cmd, $args)
    {
        $ViewHandler = new ViewHandler;
        $commands = self::_get_commands();
        if($commands[$cmd]) {
            call_user_func($commands[$cmd], $args);
        } else {
            $error = new Error("query_server_error", "unkown command '$cmd'");
            respond($error);
        }
    }
}

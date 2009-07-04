<?php

class ViewHandlerResult
{
  private static $result = array();
  static function push($value)
  {
      array_push(self::$result, $value);
  }
  
  static function reset()
  {
      self::$result = array();
  }

  static function get()
  {
      return self::$result;
  }
}

class ViewHandler
{
    private static $functions = array();
    function do_map_doc($funcsrc, $doc)
    {
        if(!function_exists("emit")) {
            function emit($key, $value)
            {
                ViewHandlerResult::push(array($key, $value));
            }
        }

        $funcmd5 = md5($funcsrc);
        $funcname = "_" . $funcmd5;

        $named_function = preg_replace("/^function/", "function $funcname", $funcsrc);

        if(!function_exists($funcname)) {
            eval($named_function);
        }
        try {
            $funcname($doc);
        } catch (Exception $e) {
            Log::line($e);
        }
    }  

    function map_doc($args)
    {
        $doc = array_shift($args);
        $response = array();

        foreach(self::$functions AS $function) {
            self::do_map_doc($function, $doc);
            $response[] = ViewHandlerResult::get();
            ViewHandlerResult::reset();
        }

        respond($response);
        respond("true");
    }

    function add_fun($args)
    {
        $code = $args[0];
        self::$functions[] = $code;
        respond("true");
    }

    function reset($args = null)
    {
        // ViewHandlerResult::reset();
        self::$functions = array();
        respond("true");
    }
}

<?php
// utils.php

// poor man's readline for PHP installs that don't have it
if(!function_exists("readline")) {
    function readline($prompt = "") // $prompt ignored
    {
        return rtrim(fgets(STDIN), "\n");
    }
}

function println($string) {
    echo "$string\n";
    Log::line($string);
    flush();
}

function respond($response)
{
    if(is_object($response) || is_array($response)) {
        println(json_encode($response));
    } elseif(is_string($response)) {
        println($response);
    } else {
        $response = new stdClass;
        $response->error = "query_server_error";
        $response->reason = "internal error: \$response is not array, object or string";
        respond($response);
    }
}

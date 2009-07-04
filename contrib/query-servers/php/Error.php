<?php

class Error
{
    function __construct($error, $reason) {
        $this->error = $error;
        $this->reason = $reason;
    }
}
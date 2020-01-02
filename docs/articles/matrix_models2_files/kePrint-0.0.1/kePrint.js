$(document).ready(function(){
<<<<<<< HEAD
    $('[data-toggle="tooltip"]').tooltip();
    $('[data-toggle="popover"]').popover();
=======
    if (typeof $('[data-toggle="tooltip"]').tooltip === 'function') {
        $('[data-toggle="tooltip"]').tooltip();
    }
    if ($('[data-toggle="popover"]').popover === 'function') {
        $('[data-toggle="popover"]').popover();
    }
>>>>>>> 9f552ce80889b6210bd6a6d1d4a7613b2dba48db
});

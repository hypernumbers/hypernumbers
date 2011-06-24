HN.timMenu = [];

HN.timMenu.reload = function() {
    console.log($(".gen_list_1").parents("[data-ref]"));
    $(".gen_list_1").parents("[data-ref]").css("overflow", "visible");
};

HN.timMenu.reload();
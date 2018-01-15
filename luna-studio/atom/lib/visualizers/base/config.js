module.exports = function (type) {
    var listVis  = type.constructor === "List" ? [{name: "table", path: "table/table.html"}] : [];
    var textVis  = type.constructor === "Text" ? [{name: "text",  path: "text/text.html"}]   : [];
    var mdVis    = type.constructor === "Text" ? [{name: "markdown",  path: "markdown/markdown.html"}]   : [];
    var yamlVis  = [{name: "yaml", path: "yaml/yaml.html"}];
    var jsonVis  = [{name: "json", path: "json/json.html"}];
    var errorVis = type.constructor === "Error" ? [{name: "error",  path: "error/error.html"}]   : [];
    return [].concat(listVis, textVis, yamlVis, jsonVis, mdVis, errorVis);
};
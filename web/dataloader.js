const document_root = '';
const infinity_cutoff = 6;
const lower_cutoff = -1;

const cost_lower_cutoff = -1
const cost_upper_cutoff = 3 
const wtp_lower_cutoff = -1
const wtp_upper_cutoff = 3

// Categories (in order that they are displayed in the legend). These are loaded from data/categories.json:
var categories;

// Default Spec. If this specification is loaded readjson will instead load default.json. This may be desirable to avoid different results (e.g. because
// different number of bootsstrap reps for default & other specs even though the currently selected spec is the default)
var default_spec = "discount_rate3tax_ratenonlinearreturns_to_schoolingIABco2_externality100etibase";

// Store the additional parameters of each policy. These are loaded from data/programs.json:
var additional_program_info

// Store the mapping of willingness to pay and government net cost in a JSON object. (loaded from data/variable_mapping)
var variable_mapping;

// Store the colors used for the Graph (loaded from data/colors.json);
var colors;

// Store additional information about the literature in an object;
var literature;

// Store a unmodified, easy to access version of all programs (Object):
var unmodified_dataset;

// This will be the array carrying the data. It should be compatible to the array returned from readcsv with the only difference that the json format omits all the keys with NA values.
// A array of this style is passed to various function. json_as_array may be modified (e.g. mvpfs larger than infinity cutoff will be censored to be 6 and 7 indicates infinity)
// To get a json_as_array_copy stores a unmodifed copy.
var json_as_array;

// Store unmodified copy of json as array. This is similar to unmodified_dataset but not an array instead of an object. If the data of a specific program
// needs to be looked up, unmodified dataset should be used because no search is required here. When initially writing all the function performance wasn't
// really a concern ^_^. Most or all instances where an ineffcient linear search was performed on the data have been replaced with time constant lookups in
// unmodified_dataset, but some functions still require that the data that is getting passed is an iterable array. With some refactoring
// I could probably get rid of json_as_array_copy but it is only a minor memory waste, and has the advantage of having an easier iterable list. Keep for now!
var json_as_array_copy;

// Object that keeps track of disabled effects
var disabled_effects = {};

// Functions
// Simple helper function which loads a url to a json file into a js object
async function loadJSON(url) {
    let json;
    await fetch(url).then(response => {
        json = response.json();
    });
    return json;
}

async function readjson(assumption_string) {
    // depends on additional_program_info being loaded as we need some way of determining which government policies exist before this function can be called
    // First we need to get a list of all programs. Where the entries are strings
    var programs = additional_program_info.map(program => {
        return program.program_identifier;
    });

    if (assumption_string === default_spec) {
        assumption_string = "default";
    }

    // Individual json requests to all of the programs
    var requests = programs.map(program => {
        // Note: usually one would expect that this code runs the loadJSON and stores the
        // result in requests. This is not what we want, as the http requests should be
        // run in parallel for better performance. Fortunately, this is not what is happening. loadJSON is a async function. And async functions return promises, which are not automatically resolved.
        return loadJSON(document_root + "data/" + program + "/" + assumption_string + ".json");
    });

    // Resolve promises in parallel
    await Promise.all(requests).then((result) => json_as_array = result);

    // This loop merges the loaded data with the details about each reform (description, sources, age etc..) form programs.json. The resulting array of objects then contains all relevant data.

    for (var i = 0; i < json_as_array.length; i++) {
        Object.assign(json_as_array[i], additional_program_info[i]);
        // Rename program identifier to program. (For compatibility with the old solution)   
        json_as_array[i]["program"] = json_as_array[i]["program_identifier"];
        delete json_as_array[i]["program_identifier"];
    }

    unmodified_dataset = {};
    for (program of json_as_array) {
        unmodified_dataset[program["program"]] = JSON.parse(JSON.stringify(program));
    }
    return json_as_array;
}

function generateAllProgramsDatasets(json_as_array, dataset_type = "scatter") {
    // This stores the name or in my context the type of program (i.e. something like education policy, tax reform ...) of all the datasets we need to construct
    // This function can be used to generate datasets for different types of charts. This mainly changes additional chart.js config that needs to be included in the dataset
    let datasets_labels = [];
    // The actual dataSets that will eventually be returned
    let datasets = [];
    let i;

    // Sort by category. This allows controlling the order in which the categories are drawn in the legend. And also yields performance benefits because it makes the datasets array that is searched as small as possible.
    json_as_array.sort(function(a, b) {
        return categories.indexOf(a.category) - categories.indexOf(b.category);
    });

    for (i = 0; i < json_as_array.length; i++) {

        // Generate a copy of the observation to prevent censoring or modification of the original data.
        let current_observation = JSON.parse(JSON.stringify(json_as_array[i]));

        // Update values if necessary:
        let updated_values = applyDisableEffects(current_observation.program);
        current_observation.mvpf = updated_values.mvpf;
        current_observation.willingness_to_pay = updated_values.willingness_to_pay;
        current_observation.government_net_costs = updated_values.government_net_costs;
        current_observation.willingness_to_pay_per_program_cost = updated_values.willingness_to_pay_per_program_cost;
        current_observation.government_net_costs_per_program_cost = updated_values.government_net_costs_per_program_cost;

        // The data for the List chart with confidence intervals needs to have a ci key which contains an array with two entries. 0: lower bound of CI and 1: upper bound of CI
        if (dataset_type === "listci_ci") {
            current_observation["mvpf_ci"] = [current_observation["mvpf_95ci_lower"], current_observation["mvpf_95ci_upper"]];
        }

        // Check if the dataset already exists. If not add it.
        if (!datasets_labels.includes(current_observation.category)) {
            datasets_labels.push(current_observation.category);
            if (dataset_type === "scatter") {
                datasets.push(generateEmptyDatasetScatter(current_observation.category, datasets.length + 1));
            }
            else if (dataset_type === "listci_pe") {
                datasets.push(generateEmptyDatasetListCIPE(current_observation.category, datasets.length + 1));
            }
            else if(dataset_type === "listci_ci") {
                datasets.push(generateEmptyDatasetListCICI(current_observation.category, datasets.length + 1));
            }
        }
        // Add current observation to the correct dataset:

        // First get the relevant dataset.
        correctDataset = datasets.find(dataset => {
            return dataset.label === current_observation.category;
        });

        // Now add the observation
        correctDataset.data.push(current_observation);
    }

    // Now the data has been read. Apply the censoring. That means convert infinite values to some number (e.g.) which will act as infinity on the chart.
    censorValues(datasets);

    return (datasets);
}

function initDisabledEffects(json_as_array) {
    for (program of json_as_array) {
        disabled_effects[program["program"]] = {
            "government_net_costs" : [],
            "willingness_to_pay" : []
        };
    }
}

function applyDisableEffects(program) {
    // Apply effect of disabling certain effects:
        // First check if something needs to be changed:
        let effect_disabled = false;

        // Initialize the value that may need updating with their original values.
        let mvpf = unmodified_dataset[program].mvpf;
        
        let government_net_costs_per_program_cost = unmodified_dataset[program].government_net_costs_per_program_cost
        let willingness_to_pay_per_program_cost = unmodified_dataset[program].willingness_to_pay_per_program_cost
        let government_net_costs = unmodified_dataset[program].government_net_costs
        let willingness_to_pay = unmodified_dataset[program].willingness_to_pay

        if (disabled_effects[program]["government_net_costs"].length > 0) {
            for (disabled_effect of disabled_effects[program]["government_net_costs"]) {
                government_net_costs -= unmodified_dataset[program][disabled_effect];
                effect_disabled = true;
            }
            government_net_costs_per_program_cost = government_net_costs / unmodified_dataset[program].program_cost;
        }

        if (disabled_effects[program]["willingness_to_pay"].length > 0) {
            for (disabled_effect of disabled_effects[program]["willingness_to_pay"]) {
                willingness_to_pay -= unmodified_dataset[program][disabled_effect];
                effect_disabled = true;
            }
            willingness_to_pay_per_program_cost = willingness_to_pay / unmodified_dataset[program].program_cost;
        }

        if (effect_disabled) {
            // Update mvpf if necessary!
            if (government_net_costs < 0 && willingness_to_pay > 0) {
                mvpf = "Inf";
            }
            else {
                mvpf = willingness_to_pay / government_net_costs;
            }
        }

        return({
            mvpf: mvpf,
            government_net_costs: government_net_costs,
            willingness_to_pay: willingness_to_pay,
            government_net_costs_per_program_cost: government_net_costs_per_program_cost,
            willingness_to_pay_per_program_cost: willingness_to_pay_per_program_cost,
            effect_disabled: effect_disabled
        })
}

function censorValues(datasets) {
    function censorMVPF(mvpf) {
        if (mvpf > infinity_cutoff) {
            return infinity_cutoff;
        }
        else if (mvpf < lower_cutoff) {
            return lower_cutoff;
        }
        else if (mvpf == "Inf") {
            return (infinity_cutoff + 1);
        }
        else {
            return mvpf;
        }
    }

    function censorCost(cost) {
        if (cost > cost_upper_cutoff ) {
            return cost_upper_cutoff;
        }
        else if (cost < cost_lower_cutoff ) {
            return cost_lower_cutoff;
        }
        else {
            return cost;
        }
    }

    function censorWTP(wtp) {
        if (wtp > wtp_upper_cutoff ) {
           return wtp_upper_cutoff;
        }
        else if (wtp < wtp_lower_cutoff ) {
            return wtp_lower_cutoff;
        }
        else {
            return wtp;
        }
    }

    for (let dataset of datasets) {
        for (let data of dataset.data) {
            data["mvpf"] = censorMVPF(data["mvpf"]);
            data["government_net_costs_per_program_cost"] = censorCost(data["government_net_costs_per_program_cost"]);
            data["willingness_to_pay_per_program_cost"] = censorWTP(data["willingness_to_pay_per_program_cost"]);
            if (data["mvpf_ci"]) {
                data["mvpf_ci"][0] = censorMVPF(data["mvpf_ci"][0]);
                data["mvpf_ci"][1] = censorMVPF(data["mvpf_ci"][1]);
            }
        }
    }
}

function generateEmptyDatasetScatter(dataset_label, dataset_number) {
    return ({
        label: dataset_label,
        data: [],
        backgroundColor: selectColor(dataset_number, true),
        borderColor: selectColor(dataset_number),
        pointHoverRadius: 7,
        pointRadius: 5
    });
}

function generateEmptyDatasetListCIPE(dataset_label, dataset_number) {
    return ({
        label: dataset_label,
        showLine: false,
        data: [],
        backgroundColor: selectColor(dataset_number),
        borderColor: selectColor(dataset_number),
        parsing: {
            xAxisKey: "mvpf",
            yAxisKey: "program_name"
        }
    });
}

function generateEmptyDatasetListCICI(dataset_label, dataset_number) {
    return ({
        label: dataset_label,
        data: [],
        backgroundColor: selectColor(dataset_number, true, 0.5),
        borderColor: selectColor(dataset_number, true, 0.5),
        parsing: {
            xAxisKey: "mvpf_ci",
            yAxisKey: "program_name"
        },
        type: "bar",
        barThickness: 10,
        borderRadius: Number.MAX_VALUE,
        borderSkipped: true,     
    });
}

function selectColor(number, background = false, overwrite_alpha_channel) {
    // returns a string in the format rgba(123,123,123,0.6)
    let background_opa = 0.7
    let foreground_opa = 0.9
    if (number > colors.length) {
        // console.log("Color not in range of supplied colors in colors.json");
        // Reuse last color in this case
        number = colors.length;
    }
    color_triple = colors[number - 1];
    if (!overwrite_alpha_channel) {
        return `rgba(${color_triple[0]},${color_triple[1]},${color_triple[2]},${background ? background_opa : foreground_opa})`;
    }
    return `rgba(${color_triple[0]},${color_triple[1]},${color_triple[2]},${overwrite_alpha_channel})`;
}

function getUnmodifiedProgram(program) {
    return(unmodified_dataset[program]);
}

function getVariableMapping(program) {
    return variable_mapping.find((current_program) => {
        return current_program.program === program;
    });
}

function generateBarChartProgramData(variable_to_plot, program, colorholder) {
    let datasets = [];
    let barComponents;
    let relevant_datapoint = getUnmodifiedProgram(program);

    if (variable_to_plot === "mvpf") {
        datasets.push({
            label: "Government Net Cost",
            data: [parseFloat(relevant_datapoint["government_net_costs"])],
            backgroundColor: selectColor(colorholder.color, true),
            borderColor: selectColor(colorholder.color),
        });
        datasets.push({
            label: "Willingness to Pay",
            data: [relevant_datapoint["willingness_to_pay"]],
            backgroundColor: selectColor(colorholder.color + 1, true),
            borderColor: selectColor(colorholder.color + 1),
        });
        colorholder.color += 2;
        return (datasets);
    }
    barComponents = getVariableMapping(program)[variable_to_plot];

    for (let component in barComponents) {
        datasets.push({
            label: barComponents[component],
            effect_key: component,
            data: [relevant_datapoint[component]],
            backgroundColor: selectColor(colorholder.color, true),
            borderColor: selectColor(colorholder.color),
            borderRadius: 4
        });
        colorholder.color++;
    }
    return (datasets);
}

function mvpfToString(mvpf) {
    if (mvpf == "Inf") {
        return "∞";
    }
    else if (mvpf == "-Inf") {
        return "-∞";
    }
    else {
        return parseFloat(mvpf).toFixed(2);
    }
}

// Trigger load of the default dataset:
async function loadData() {
    await Promise.all([
        loadJSON(document_root + "data/programs.json"),
        loadJSON(document_root + "data/categories.json")
    ]).then(return_values => {
        additional_program_info = return_values[0];
        categories = return_values[1];
        return Promise.all([
            readjson("default"),
            loadJSON(document_root + "data/variable_mapping.json"),
            loadJSON(document_root + "data/colors.json"),
            loadJSON(document_root + "data/literature.json")
        ]);
    }).then((return_values) => {
        let json = return_values[0];
        variable_mapping = return_values[1];
        colors = return_values[2];
        literature = return_values[3];
        json_as_array_copy = JSON.parse(JSON.stringify(json));
        initDisabledEffects(json);
    });
}
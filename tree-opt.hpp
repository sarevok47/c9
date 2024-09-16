#pragma once

#include "cfg.hpp"


namespace c9 { namespace tree_opt {
void cse      (cfg::control_flow_graph &cfg);
void constprop(cfg::control_flow_graph &cfg);
void dce      (cfg::control_flow_graph &cfg);
void dse      (cfg::control_flow_graph &cfg);
}}

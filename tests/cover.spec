%% -*- mode: erlang -*-

%% List of Nodes on which cover will be active during test.
%% Nodes = [atom()]
{nodes, [arrian@localhost]}.       

%% Files with previously exported cover data to include in analysis.
%% CoverDataFiles = [string()]
{import, ["tests/cover.data"]}.

%% Cover data file to export from this session.
%% CoverDataFile = string()
{export, "tests/cover.data"}.

%% Cover analysis level.
%% Level = details | overview
{level, details}.       

%% TODO: Generate this programatically?
%% Should only include hypernumbers modules for now
{incl_mods, [hn_db_api, 
             hn_db_wu,
             hn_mochi,
             dirty_sup]}.

%% Directories, including subdirectories, to include.
%{incl_dirs_r, Dirs}

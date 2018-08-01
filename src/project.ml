open Printf
open Yojson.Basic.Util
open Utils

let gen_lblox_topo json_topo =
  let mbox_insts = json_topo |> member "middleboxInstances" |> to_list in
  let mbox_kinds = mbox_insts |> List.map (fun entry -> entry |> member "kind" |> to_string ) in
  let aliases = mbox_kinds 
                |> List.map (fun kind -> (sprintf 
                                            "    alias_all(`%s:%s_mbox)," 
                                            kind kind))
                |> String.concat "\n"
  in
    
  let instances = mbox_insts 
                  |> List.map (fun inst -> let kind = inst |> member "kind" |> to_string in 
                                           let id = inst |> member "id" |> to_int in
                                           sprintf "
        %s(mbox) <-
            middleboxId[%d]=mbox.

    "
                              kind id)
                  |> String.concat "\n"

  in
  let mbox_mbox_connections = json_topo |> member "middleboxLinks" |> to_list
                              |> List.map (fun link -> sprintf "
        link(mbox_1,port_1,mbox_2,port_2) <-
            portId[%d]=port_1,
            portId[%d]=port_2,
            middleboxId[%d]=mbox_1,
            middleboxId[%d]=mbox_2.

    "
                                                               ( link |> member "port1Id" |> to_int )
                                                               ( link |> member "port2Id" |> to_int )
                                                               ( link |> member "mbox1Id" |> to_int )
                                                               ( link |> member "mbox2Id" |> to_int )
                                          ) 
                              |> String.concat "\n"
  in

  let mbox_host_connections = json_topo |> member "connectedHosts" |> to_list
                              |> List.map (fun link -> sprintf "

        connected_hosts(mbox, port, h) <-
            middleboxId[%d]=mbox,
            portId[%d]=port,
            int:range(%d,%d,1,x),
            hostId[x]=h.

    "
                                                               ( link |> member "mboxId" |> to_int )
                                                               ( link |> member "portId" |> to_int )
                                                               ( link |> member "fromHost" |> to_int )
                                                               ( link |> member "toHost" |> to_int )
                                        )                                                               
                              |> String.concat "\n"                                               
  in

  let can_send = json_topo |> member "availablePackets" |> to_list
                 |> List.map (fun link -> sprintf "

        can_send(src, dst, tpe) <-
            int:range(%d,%d,1,x),
            hostId[x]=src,
            int:range(%d,%d,1,y),
            hostId[y]=dst,
            int:range(%d,%d,1,z),
            typeId[z]=tpe.

    "
                                                  ( link |> member "srcFrom" |> to_int )
                                                  ( link |> member "srcTo" |> to_int )
                                                  ( link |> member "dstFrom" |> to_int )
                                                  ( link |> member "dstTo" |> to_int )
                                                  ( link |> member "typeFrom" |> to_int )
                                                  ( link |> member "typeTo" |> to_int )
                                                  
                             )                           
                 |> String.concat "\n"                                               
in
    sprintf "
block(`topo) {
    alias_all(`core:network),
%s

    clauses(`{
        // Middlebox instantiations
%s

        // Network topology
        //  mbox-mbox connections
%s
        
        //  mbox-host connections
%s

        // Packets Each host can send
%s
        
    })
} <-- .
"
            aliases
            instances
            mbox_mbox_connections
            mbox_host_connections
            can_send

let gen_lblox_data json_topo =

    let typeCount = json_topo |> member "typeCount" |> to_int in
    let hostMaxId = json_topo |> member "connectedHosts" |> to_list
                    |> List.map (fun conn -> conn |> member "toHost" |> to_int)
                    |> List.fold_left max 0 in
    let portMaxId = max ( json_topo |> member "connectedHosts" |> to_list
                          |> List.map (fun conn -> conn |> member "portId" |> to_int)
                          |> List.fold_left max 0 )
                        ( json_topo |> member "middleboxLinks" |> to_list
                          |> List.map (fun link -> ( link |> member "port1Id" |> to_int ) 
                                                   :: (link |> member "port2Id" |> to_int) 
                                                   :: [] )
                          |> List.flatten |> List.fold_left max 0 ) in
    let middleboxCount = json_topo |> member "middleboxInstances" |> to_list |> List.length in
    sprintf "

+core:network:host(y), +core:network:hostId[x]=y <-
    int:range(0,%d,1,x).

+core:network:port(y), +core:network:portId[x]=y <-
    int:range(0,%d,1,x).

+core:network:type(y), +core:network:typeId[x]=y <-
    int:range(0,%d,1,x).
    
+core:network:middlebox(y), +core:network:middleboxId[x]=y <-
    int:range(0,%d,1,x).
	
"
            hostMaxId
            portMaxId
            ( typeCount - 1 )
            ( middleboxCount - 1 )

let gen_core_network_file =
  "
block(`network) {
    export(`{
        host(_) -> .
        type(_) -> .
        port(_) -> .
        middlebox(_) -> .

        hostId[x]=y -> int(x), host(y).
        typeId[x]=y -> int(x), type(y).
        portId[x]=y -> int(x), port(y).
        middleboxId[x]=y -> int(x), middlebox(y).

        pending(mbox,src,dst,tp,pr) ->
            middlebox(mbox), host(src), host(dst), type(tp), port(pr).

        link(mbox1,port1,mbox2,port2) ->
            middlebox(mbox1), port(port1), middlebox(mbox2), port(port2).

        connected_hosts(mbox, prt, h) ->
            middlebox(mbox), port(prt), host(h).

        can_send(src, dst, tpe) ->
            host(src), host(dst), type(tpe).

        abort(mbox) -> middlebox(mbox).
    }),
    clauses(`{
        lang:constructor(`hostId).
        lang:constructor(`typeId).
        lang:constructor(`portId).
        lang:constructor(`middleboxId).

        link(mbox1,port1,mbox2,port2) <-
            link(mbox2,port2,mbox1,port1).

        pending(mbox,src,dst,tp,prt) <-
            connected_hosts(mbox, prt, src),
            can_send(src, dst, tp).
    })
} <-- ."


let gen_lblox_config_file projectdir =
    sprintf "

from lbconfig.api import *

lbconfig_package('application', version='0.1',
                 default_targets=['lb-libraries'])

depends_on(logicblox_dep)

lb_library(name='application', srcdir='.')

check_lb_workspace(name='%s', libraries=['application'])
    
"
            projectdir
    
let gen_compilation_script =
    "#!/bin/bash

export LB_DEPLOYMENT_HOME=/home/ubuntu/tmp/lb-dep-home
source ~/bin/logicblox-x86_64-linux-4.2.1-1fb88d1345f4/etc/profile.d/logicblox.sh

lb config                      ; sleep 0.2
make                           ; sleep 0.2
"

let gen_logicblox_run_script projectdir =
    sprintf "#!/bin/bash

export LB_DEPLOYMENT_HOME=/home/ubuntu/tmp/lb-dep-home
source ~/bin/logicblox-x86_64-linux-4.2.1-1fb88d1345f4/etc/profile.d/logicblox.sh

echo ===============================================
echo == Running StateSafe on the given network
echo ==   

time make check-ws-%s         ; sleep 0.2

echo
echo

echo == Run results:
echo == 

lb query.lb                    ; sleep 0.2

echo
echo

echo == Cleanup:
echo == 

make clean                     ; sleep 0.2

echo
echo ===============================================
echo
echo

"
            projectdir

let gen_run_wrapper =
  "
#!/bin/bash

nohup ./run_logicblox.bash &> run_log.out &
"

let gen_query_script projectdir =
  sprintf "

open %s

echo

echo List of middlebox references that have reached an error state:
echo -----------------------------------------
print core:network:abort
echo -----------------------------------------
echo Note: if nothing appears between the lines above, congratulations, your network is SAFE.
echo
echo

exit

    "
          projectdir
          

let gen_project_file mbox_files =
  sprintf 
    "
amdl_project, projectname
core, module
%s
topology, module
data/init_data.logic, execute
"
    (List.map (fun fname -> sprintf "%s, module" (Filename.basename fname |> Filename.remove_extension))mbox_files 
     |> String.concat "\n")



let create_project mbox_files topo = 

    let core_network = open_file_in_dir "core" "network.logic" in 
    let topology_topo = open_file_in_dir "topology" "topo.logic" in 
    let data_init_data = open_file_in_dir "data" "init_data.logic" in
    let config_f = open_out "config.py" in 
    let comp_f = open_out "compile_logicblox.bash" in 
    let run_f = open_out "run_logicblox.bash" in 
    let wrap_f = open_out "run_wrapper.bash" in 
    let query_f = open_out "query.lb" in
    let proj_dir = Sys.getcwd () |> Filename.basename in
    let proj_f = open_out "application.project" in 
    
    gen_core_network_file |> output_string core_network;
    gen_lblox_topo topo |> output_string topology_topo;
    gen_lblox_data topo |> output_string data_init_data;
    gen_lblox_config_file proj_dir |> output_string config_f;
    gen_compilation_script |> output_string comp_f;
    gen_logicblox_run_script proj_dir |> output_string run_f;
    gen_run_wrapper |> output_string wrap_f;
    gen_query_script proj_dir |> output_string query_f;
    gen_project_file mbox_files |> output_string proj_f;


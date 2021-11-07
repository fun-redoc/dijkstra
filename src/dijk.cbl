       replace ==:err999:== by ==999==
               ==:err01-invalid-node-name:==  by ==001==
               ==:err02-queue-full:==         by ==002==
               ==:err-stack-overflow:==       by ==003==
               ==:err-empty-stack:==          by ==004==
               ==:err-tree-full:==            by ==005==
               ==:err-invalid-node-id:==      by ==006==
               ==:err-too-many-nodes:==       by ==007==
               ==:err-too-many-iterations:==  by ==008==
               ==:err-node-id-too-long:==     by ==009==
               ==:err-node-name-too-long:==   by ==010==
               ==:err-node-line-too-long:==   by ==011==
               ==:err-edge-weight-too-long:== by ==012==
               ==:err-edge-line-too-long:==   by ==013==
               ==:nodes-region:==             by =="[nodes]"==
               ==:edges-region:==             by =="[edges]"==
               ==:end-region:==               by =="[end]  "==
               ==:node-name-len:==            by ==76==
               ==:node-id-len:==              by ==4==
               ==:edge-weight-len:==          by ==3==
               ==:maxlen:==                   by ==4==
               ==:maxval:==                   by ==999==
               ==:max-csv-cols:==             by ==3==
               ==:dijkstra-tab-len:==         by ==999==
               ==:graph-max-nodes:==          by ==999==
               ==:node-id-type:==             by ==999==
               ==:graph-max-successors:==     by ==99==
               ==:start-dest-distance-type:== by ==999==
               ==:dijkstra-max-iterations:==  by ==9999==
               .
      **
      *    dijkstra algorithm in colbol
      **
       IDENTIFICATION DIVISION. 
       PROGRAM-ID. DIJK.
       ENVIRONMENT DIVISION.
      *
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO INFILE
                          FILE STATUS IS PROG-STATUS.
           SELECT outfile ASSIGN TO outfile
                          FILE STATUS IS PROG-STATUS.
      *
       DATA DIVISION.
      *
       FILE SECTION.
       FD INFILE RECORD CONTAINS 80 CHARACTERS
                 RECORDING MODE IS F.
       01 fd-INFILE-REC PIC X(80).

       FD outfile RECORD CONTAINS 80 CHARACTERS
                  RECORDING MODE IS F.
       01 fd-outfile-REC PIC X(80).

       WORKING-STORAGE SECTION. 
       01 PROG-STATUS PIC 99 VALUE 0.
       
       01 FILE-STATUS PIC XXX VALUE SPACES.
       88 FILE-STATUS-EOF VALUE 'EOF'.

       01  sysin.
           02 sysin-from pic x(:node-name-len:).
           02 sysin-to pic x(:node-name-len:).

       01 infile-defs.
           10 infile-rec pic x(80).
           10 infile-label-region redefines infile-rec.
              15 infile-label pic x(7).
              15 dummy        pic x(73).
           10 infile-nodes-region redefines infile-rec.
              15 infile-node-id pic x(4).
              15 infile-node-name pic x(:node-name-len:).
           10 infile-edges-region redefines infile-rec.
              15 infile-start-node-id pic x(4).
              15 infile-dest-node-id pic x(4).
              15 infile-edge-cost pic 9(3).
              15 dummy            pic x(69).

       01 graph-read-states.
          10 graph-read-state pic X.
          88 graph-read-state-start value 'X'.
          88 graph-read-state-nodes value 'N'.
          88 graph-read-state-edge  value 'E'.
          88 graph-read-state-ready value 'R'.
          88 graph-read-state-error value 'F'.

       01 b100-read-graph-data.
           05 b100-locals.
              10 b100-col pic 99.
              10 b100-lineno pic 99.

       01 dijkstra-algorith-states.
          10 dijkstra-state pic X.
          88 dijkstra-state-error value 'F'.


       01 graph.
           15 max-node-idx usage index.
           15 max-edge-idx usage index.
           15 num-nodes pic 9(4).
           15 graph-nodes OCCURS 999 INDEXED BY nidx.
              20 graph-node-id   pic 9(:node-id-len:).
              20 graph-node-name pic x(:node-name-len:).
           15 graph-edges occurs 999 INDEXED BY eidx.
              20 graph-edge-from-idx usage index.
              20 graph-edge-from-id  pic 9(4).
              20 graph-edge-to-idx   usage index.
              20 graph-edge-to-id    pic 9(4).
              20 graph-edge-weight   pic 9(:edge-weight-len:).
           15 graph-adjacense-matrix.
              20 from-i pic 999.
              20 to-i pic 999.
              20 adj-from-node occurs 999.
                 25 adj-to-node occurs 999.
                    30 edge-idx usage index.
                    30 edge-existance pic x value spaces.
                    88 edge-exists value 'X'.
                    88 edge-doesnt-exists value space.

       01 d100-dijkstra-struc.
          10 d100-dijkstra-result.
            15 d100-res-path occurs :graph-max-nodes:
                             indexed by d100-path-idx d100-max-path-idx.
              20 d100-res-id   pic 9(4).
              20 d100-res-name pic x(:node-name-len:).
              20 d100-res-dist pic :start-dest-distance-type:.
          10 d100-dijkstra-tab occurs :dijkstra-tab-len:
                               indexed by d100-idx d100-max-idx.
             15 d100-iteration pic :dijkstra-max-iterations:.
             15 d100-node occurs :graph-max-nodes:
                               indexed d100-node-idx d100-max-node-idx.
               20 d100-dist pic :start-dest-distance-type:.
               20 d100-pred-subs pic 9(4).
          10 d100-ready-nodes occurs :graph-max-nodes:
                              indexed by d100-ready-idx 
                                         d100-max-ready-idx.
             15 d100-ready-node-subs pic 9(4).
          10 d100-successor-nodes occurs :graph-max-successors:
                              indexed d100-succ-idx d100-max-succ-idx.
             15 d100-succ-node-subs pic 9(4).
          10 d100-current-node-subs pic 9(4).
          10 d100-subs pic 9(4).
          10 d100-next-iteration pic :dijkstra-max-iterations:.

          10 d300-dijkstra-prep.
             15 d300-dijkstra-start-idx usage index.
             15 d300-dijkstra-start-subs pic 9(4).
             15 d300-dijkstra-start-id pic 9(4).
             15 d300-dijkstra-dest-idx usage index.
             15 d300-dijkstra-dest-subs  pic 9(4).
             15 d300-dijkstra-dest-id  pic 9(4).

          10 d400-dijkstra-run-data.
             15 d400-dist      pic :start-dest-distance-type:.
             15 d400-new-dist  pic :start-dest-distance-type:.
             15 d400-last-dist pic :start-dest-distance-type:.
             15 d400-tota-dist pic :start-dest-distance-type:.
             15 d400-last-subs pic 9(4).

          10 d500-write-result-data.
             15 d500-output pic x(80).
             15 d500-output-ptr pic 99.

          10 d900-dijkstra-trace-local.
             15 d900-step pic :dijkstra-tab-len:.
             15 d900-node-no pic 999.
             15 d900-node-name pic x(:node-name-len:).
             15 d900-output pic x(80).
             15 d900-out-dist pic zzz9.
             15 d900-output-ptr pic 99.
             15 d900-output-ptr1 pic 99.
             15 d900-subs pic 9(4).

       01 c100-parse-csv-line-data.
           10 c100-in-attirbs.
              15 c100-in-str pic x(80).
           10 c100-out-attribs.
              15 c100-out-tab occurs :max-csv-cols:
                              indexed c100-cidx 
                                      c100-max-cidx.
                 20 c100-out-start-ptr pic 99.
                 20 c100-out-len pic 99.
           10 c100-locals.
              15 c100-instr-ptr pic 99.
              15 c100-i pic 99.
              15 c100-l pic 99.

       01 q100-priority-queue-struc.
           *> uses binary tree t100 as data structure
           05 q110-new-entry.
              *> refactor queue and tree the type is too strict
              10 q110-new-entry-id pic 9(4). *> pay attation on the type 
              10 q110-new-entry-weight pic 9(4).
           05 q120-result-struc.
              15 q120-result pic x.
              88 q120-result-ok value 'x'.
              88 q120-result-q-empty value 'e'.
              15 q120-next-entry.
                 25 q120-next-entry-id pic 9(4).
                 25 q120-next-entry-weight pic 9(4).

       01 z100-search-node-by-name.
          05 z100-search-nd-nme pic x(:node-name-len:).
          05 z100-i pic 9(4).
          05 z100-res-node-id pic 9(4).
          05 z100-res-node-subscript pic 999.
          05 z100-res-node-idx usage index.

       01 z200-search-node-by-id.
          05 z200-search-nd-id pic 9(4).
          05 z200-i pic 9(4).
          05 z200-res-node-subscript pic 999.
          05 z200-res-node-idx usage index.

       01 z300-display-adj-matrix.
          05 z300-output pic x(80).
          05 z300-name   pic x(4).
          05 z300-weight pic 9(4) usage display.
          05 z300-ptr pic 99.
          05 z300-hline-len pic 99.
       
       01 z400-trim-string-data.
          05 z400-args.
             15 z400-in-string pic x(80).
             15 z400-out-start-ptr pic 99.
             15 z400-out-len pic 99.
          05 z400-locals.
             15 z400-left-trim-pos pic 99.
             15 z400-total-len pic 99.


       01 z900-display-hline-struc.
          10 z900-interface-vars.
             15 z900-line-len pic 99.
          10 z900-locals.
             15 z900-i pic 99.
             15 z900-output pic x(80).
       
       01 s100-stack-struc.
           05 s100-stack occurs 999 
                         indexed s100-top-idx s100-bottom-idx. 
              10 s100-stack-elem pic 9(3).
       01 s110-elem pic 9(3).
       01 s120-elem pic 9(3).
       01 s130-stack-empty-val pic x.
       88 s130-stack-empty value 'Y'.
       88 s130-stack-not-empty value 'N'.

       01 r100-random-generator-struc.
           10 curdate.
             15 curdate-num-part pic 9(16).
             15 curdate-rest pic x(5).
           10 rnd.
             15 frnd pic 9V99999999999 value zeroes.
             15 r110-ret-irnd pic 9(:maxlen:) value zeroes.

       01 t100-binary-tree-struc.
           05 t100-root pic 999.
           05 t100-next-free-pos pic 999 value 0.
           05 t100-bt occurs 999.
              10 t100-bt-entry.
                 15 t100-entry-id pic 9(4).
                 15 t100-entry-weight pic 9(4).
              10 t100-bt-left pic 999 value 0.
              10 t100-bt-right pic 999 value 0.
              
       01 t110-bt-insert-struc.
          10 t110-bt-in-parms.
             15 t110-entry-id pic 9(4).
             15 t110-entry-weight pic 9(4).
          10 t110-bt-local.
             15 t110-cur-nd pic 999.
             15 t110-last-nd pic 999.
             15 t110-do-insert-flag pic x.
             88 t110-do-insert value 'x'.
             88 t110-dont-insert value '-'.

       01 t120-bt-del-struc.
          10 t120-bt-in-parms.
             15 t120-del-entry-id pic 9(4).
             15 t120-result pic x.
             88 t120-result-ok value 'x'.
             88 t120-result-not-found value 'n'.
          10 t120-bt-locals.
             15 t120-cur-pos pic 999.
             15 t120-del-elem-pos pic 999.
             15 t120-next-pos pic 999.
             15 t120-found-pos pic 999.
             10 t120-bt-node.
               20 t120-bt-entry.
                  25 t120-entry-id pic 9(4).
                  25 t120-entry-weight pic 9(4).
               20 t120-bt-left pic 999 value 0.
               20 t120-bt-right pic 999 value 0.

       01 t130-bt-pop-min-struc.
          10 t130-bt-pop-min-result-data.
             15 t130-entry-id pic 9(4).
             15 t130-entry-weight pic 9(4).
          10 t130-bt-pop-min-result pic x.
          88 t130-bt-pop-min-res-ok value 'x'.
          88 t130-bt-pop-min-res-not-found value 'n'.
          10 t130-bt-locals.
             15 t130-cur-pos pic 999.
             15 t130-prev-pos pic 999.
             15 t130-found-pos pic 999.

       01 t910-pos pic 9(3).

       01 t990-pos pic 9(3).

       01 a100-locals.
           05 a100-i pic 9(3).

       LINKAGE SECTION. 
       01  PARM-BUFFER.
          05  PARM-LENGTH         pic S9(4) comp.
          05  PARM-DATA           pic X(256).

       PROCEDURE DIVISION using PARM-BUFFER.
      *----------------------------------------------------------------
       DECLARATIVES.
       DEBUG SECTION.
             USE FOR DEBUGGING ON ALL PROCEDURES.
       DEBUG-DECLARATIVES-PARAGRAPH.
             DISPLAY '>> ', DEBUG-ITEM.
       END DECLARATIVES.
      *----------------------------------------------------------------
       a000-main section.
      *    Display 'start of program'
           move "  3456789 123456 89 12" to z400-in-string
           PERFORM z400-trim-string
           
           *> recieve start and destination from sysin
           accept sysin-from
           accept sysin-to
           
           open input INFILE
           if PROG-STATUS = 0
              open output outfile
              if prog-status = 0
                 perform b100-read-graph                                read gr
                 if graph-read-state-error                           
                    perform a900-write-read-error
                 else
      *             perform z300-display-adj-matrix-proc
                    perform d100-dijkstra                               dikstra
                    if PROG-STATUS = 0
                       perform d500-write-result                        write o
                    else
                       perform a910-dikstra-error
                    end-if
                 end-if
                 close outfile
              end-if
              close infile
           end-if

           MOVE PROG-STATUS TO RETURN-CODE

      *    Display 'end of program'

           goback.
      *    ** END of MAIN **
      ******************************************************************
      *    A
      ******************************************************************
       a900-write-read-error section.
           *> TODO
           continue.
       a910-dikstra-error section.
           *> TODO
           continue.
      ******************************************************************

      ******************************************************************
      *    read graph
      ******************************************************************
       b100-read-graph section.
           *> read graph is implemented as a state automaton
           *>    state-start  [nodes]    -> state-nodes
           *>    state-nodes  otherwiese -> state-nodes
           *>                 [edges]    -> state-edges   
           *>    state-edges  otherwise  -> state-edges
           *>                 [end]      -> state-ready
           *>    state-error can alwasy be entered in case of error
           *>                additionall prog-status is set to the
           *>                appropriate number (via symbol replacing)
           *>                and a message is displayed
           set graph-read-state-start to true
           INITIALIZE FILE-STATUS
           move 1 to b100-lineno
           perform until FILE-STATUS-EOF
                      or graph-read-state-error
                      or graph-read-state-ready 
              read infile into INFILE-REC
               at end 
                 set FILE-STATUS-EOF to true
               not at END
                 EVALUATE true
                     WHEN graph-read-state-start and 
                          infile-edges-region = :nodes-region:
      *                 *> [nodes] badge
                        set nidx to 1
                        move 0 to num-nodes
                        set graph-read-state-nodes to true
                     When graph-read-state-nodes AND 
                          infile-edges-region =  :edges-region:
      *                 *>[edges] badge
                        set eidx to 1
                        INITIALIZE graph-adjacense-matrix 
                        set graph-read-state-edge to true
                     When graph-read-state-edge AND 
                          infile-edges-region =  :end-region:
                        *> [end] badge reached finalize all
                        set max-node-idx to nidx
                        set max-edge-idx to eidx
                        set graph-read-state-ready to true
                     When graph-read-state-nodes
                        perform b110-read-graph-node
                        *> next node
                        set nidx up by 1
                        add 1 to num-nodes
                     When graph-read-state-edge
                        perform b120-read-graph-edge
                        perform b125-insert-to-adj-matrix
                        *> next exge
                        set eidx up by 1
                     WHEN OTHER
      *                 display " error "
                        set graph-read-state-error to true
                        CONTINUE
                 END-EVALUATE
              END-READ
              add 1 to b100-lineno
           end-perform
           continue.
       
       b110-read-graph-node section.
           move infile-rec to c100-in-str
           perform c100-parse-csv-line
           move 1 to b100-col
           perform varying c100-cidx from 1 by 1
                     until c100-cidx > c100-max-cidx 
                        or graph-read-state-error
              evaluate b100-col
                 when 1 *> id column
                   if c100-out-len(c100-cidx) <= :node-id-len:
                      move infile-rec (c100-out-start-ptr(c100-cidx)
                                      :c100-out-len(c100-cidx))
                        to graph-node-id(nidx) 
                   else
                      perform b190-err-node-id-too-long
                   end-if
                 when 2 *> name column
                   if c100-out-len(c100-cidx) <= :node-name-len:
                      move infile-rec (c100-out-start-ptr(c100-cidx)
                                      :c100-out-len(c100-cidx))
                        to graph-node-name(nidx) 
                   else
                      set graph-read-state-error to true
                      move :err-node-name-too-long: 
                        to prog-status
                      display "ERROR: node name has len " 
                      display "       " c100-out-len (c100-cidx)
                      display "       but maxmial is" 
                      display "       " :node-name-len:
                      display "       in line " b100-lineno 
                   end-if
                 when OTHER 
                      set graph-read-state-error to true
                      move :err-node-line-too-long: 
                        to prog-status
                      display "ERROR: to many columns " 
                      display "       in line " b100-lineno 
              END-EVALUATE 
              add 1 to b100-col
           end-perform
           *> TEST
           *> display "-->" graph-node-id(nidx) "<-->"
           *>               graph-node-name(nidx) "<"
           *> TEST END
           continue.

       b120-read-graph-edge section.
           move infile-rec to c100-in-str
           perform c100-parse-csv-line
           move 1 to b100-col
           perform varying c100-cidx from 1 by 1
                     until c100-cidx > c100-max-cidx 
                        or graph-read-state-error
              evaluate b100-col
                 when 1 *> from id column
                   if c100-out-len(c100-cidx) <= :node-id-len:
                      move infile-rec (c100-out-start-ptr(c100-cidx)
                                      :c100-out-len(c100-cidx))
                        to graph-edge-from-id(eidx) 
                      move graph-edge-from-id(eidx) to z200-search-nd-id
                      perform z200-search-node-by-id-proc
                      set graph-edge-from-idx(eidx) to z200-res-node-idx
                   else
                      perform b190-err-node-id-too-long
                   end-if
                 when 2 *> to id column
                   if c100-out-len(c100-cidx) <= :node-id-len:
                      move infile-rec (c100-out-start-ptr(c100-cidx)
                                      :c100-out-len(c100-cidx))
                        to graph-edge-to-id(eidx) 
                      move graph-edge-to-id(eidx) to z200-search-nd-id
                      perform z200-search-node-by-id-proc
                      set graph-edge-to-idx(eidx) to z200-res-node-idx
                   else
                      perform b190-err-node-id-too-long
                   end-if
                 when 3 *> distance / weight column
                   if c100-out-len(c100-cidx) <= :edge-weight-len:
                      move infile-rec (c100-out-start-ptr(c100-cidx)
                                      :c100-out-len(c100-cidx))
                        to graph-edge-weight(eidx) 
                   else
                      set graph-read-state-error to true
                      move :err-edge-weight-too-long: 
                        to prog-status
                      display "ERROR: edge weight has len " 
                      display "       " c100-out-len (c100-cidx)
                      display "       but maxmial is" 
                      display "       " :edge-weight-len:
                      display "       in line " b100-lineno 
                   end-if
                 when OTHER 
                      set graph-read-state-error to true
                      move :err-edge-line-too-long: 
                        to prog-status
                      display "ERROR: to many columns " 
                      display "       in line " b100-lineno 
              END-EVALUATE 
              add 1 to b100-col
           end-perform
           *> TEST
           *> display "--> FROM:" graph-edge-from-id(eidx) ":TO:"
           *>               graph-edge-to-id(eidx) ":WEIGHT:"
           *>               graph-edge-weight(eidx) ":"
           *> TEST END

           continue.
       
       b125-insert-to-adj-matrix section.
           move graph-edge-from-id(eidx) to from-i 
           move graph-edge-to-id(eidx) to to-i
           set edge-idx(from-i to-i) to eidx
           set edge-exists(from-i to-i) to true
           continue.

       b190-err-node-id-too-long section.
           set graph-read-state-error to true
           move :err-node-id-too-long: 
             to prog-status
           display "ERROR: node id has len " 
           display "       " c100-out-len (c100-cidx)
           display "       but maxmial is" 
           display "       " :node-id-len:
           display "       in line " b100-lineno 
           continue.
      ******************************************************************

      ******************************************************************
      *    dijkstra algorithen
      ******************************************************************
       d100-dijkstra section.
           if num-nodes > :graph-max-nodes:
              *> ERROR dijkstra tab to small
              display "ERROR: implementation can only handle "
              display "       " :graph-max-nodes: " nodes "
              display "       but got " num-nodes "."
              move :err-too-many-nodes: to prog-status
           else
              perform d200-dijkstra-init
              if prog-status = 0
                 perform d400-dikstra-run
              end-if
           end-if
           continue.

       d200-dijkstra-init section.
           set d100-node-idx to 1
           set d100-max-node-idx to 1
           set d100-succ-idx to 1
           set d100-max-succ-idx to 1
           set d100-max-ready-idx to 1
           move 0 to d100-next-iteration
           perform q100-prio-queue-init
           perform d300-dijkstra-prep-start-dest
           move d300-dijkstra-start-subs to d100-current-node-subs

           *> update dijkstratab
           set d100-max-idx to 1
           move d100-next-iteration to d100-iteration(d100-max-idx)
           set d100-max-node-idx to 1
           perform VARYING d100-subs from 1 by 1 
                                     until num-nodes < d100-subs 
              *> hier geht es weiter
              if d100-subs = d100-current-node-subs
                 move 0 to d100-dist(d100-max-idx d100-max-node-idx)
                 move 0 
                   to d100-pred-subs(d100-max-idx d100-max-node-idx)
              else
                 move :start-dest-distance-type: 
                   to d100-dist(d100-max-idx d100-max-node-idx) *> max value
                 move 0 
                   to d100-pred-subs(d100-max-idx d100-max-node-idx)
              end-if
              set d100-max-node-idx up by 1
           end-perform
           *> enqueue start node
           move 0 to q110-new-entry-weight
           move d100-current-node-subs to q110-new-entry-id
           perform q110-prio-queue-insert
           *> prepare for next step
           if d100-next-iteration < :dijkstra-max-iterations:
              add 1 to d100-next-iteration
              set d100-max-idx up by 1
      *       *> gather successor nodes
      *       set d100-max-succ-idx to 1
      *       perform varying d100-subs from 1 by 1
      *                  until num-nodes < d100-subs 
      *        if edge-exists(d100-current-node-subs d100-subs)
      *          move d100-subs 
      *            to d100-succ-node-subs(d100-max-succ-idx)
      *          set d100-max-succ-idx up by 1
      *        end-if
      *       end-perform
              move d100-current-node-subs to q110-new-entry-id
           else
              display "ERROR: max number of iteration"
              display "       " :dijkstra-max-iterations:
              display "       exceeded"
              move :err-too-many-iterations: to prog-status  
           end-if
           continue.

       d300-dijkstra-prep-start-dest section.
           move sysin-from to z100-search-nd-nme
           perform z100-search-node-by-name-proc 
           if PROG-STATUS not = 0
              display "ERROR: start node for name " z100-search-nd-nme
              display " not found."
           else
              move z100-res-node-id to d300-dijkstra-start-id
              set d300-dijkstra-start-idx to z100-res-node-idx
              move z100-res-node-subscript 
                to d300-dijkstra-start-subs
              move sysin-to to z100-search-nd-nme
              perform z100-search-node-by-name-proc 
              if PROG-STATUS not = 0
                 display "ERROR: dest node for name " z100-search-nd-nme
                 display " not found."
              else
                 move z100-res-node-id to d300-dijkstra-dest-id
                 set d300-dijkstra-dest-idx to z100-res-node-idx
                 move z100-res-node-subscript
                   to d300-dijkstra-dest-subs 
              end-if
           end-if
           continue.
      ******************************************************************
      *    dijkstra run
      ******************************************************************
       d400-dikstra-run section.
           *> receive current node from priority queue
           perform q120-prio-queue-pop
           perform test before until q120-result-q-empty
              *> poped least distance node is current node
              move q120-next-entry-id     to d100-current-node-subs
              move q120-next-entry-weight to d400-dist
      *       perform d900-dijkstra-trace 

              *> check if current node allready checked to avoid loops
              *> TODO

              set d100-idx to d100-max-idx
              set d100-idx down by 1 *> access last iteration
              *> crate new iteration line
              move d100-dijkstra-tab(d100-idx) 
                to d100-dijkstra-tab(d100-max-idx)
              move d100-next-iteration to d100-iteration(d100-max-idx)

              *> find successors
              set d100-max-succ-idx to 1
              perform varying d100-subs from 1 by 1
                         until num-nodes < d100-subs 
               if edge-exists(d100-current-node-subs d100-subs)
                 set eidx
                  to edge-idx(d100-current-node-subs d100-subs)
                 compute d400-new-dist
                       = d400-dist + graph-edge-weight(eidx)
                 compute d400-last-dist 
                       = d100-dist(d100-max-idx d100-subs)
                 if d400-new-dist < d400-last-dist
                    move d100-subs 
                      to d100-succ-node-subs(d100-max-succ-idx)
                    set d100-max-succ-idx up by 1
                    compute d100-dist(d100-max-idx d100-subs)
                          = d400-new-dist
                    move d100-current-node-subs 
                      to d100-pred-subs(d100-max-idx d100-subs)
                 end-if
               end-if
              end-perform

              *> current node is ready, remeber
              move d100-current-node-subs 
                to d100-ready-node-subs(d100-max-ready-idx)

              *> enqueue successors calculate total dists sofar
              perform varying d100-succ-idx from 1 by 1
                        until d100-max-succ-idx <= d100-succ-idx
                move d100-succ-node-subs(d100-succ-idx) 
                  to d100-subs
                move d100-dist(d100-max-idx d100-subs)
                  to q110-new-entry-weight
                move d100-succ-node-subs(d100-succ-idx) 
                  to q110-new-entry-id
                perform q110-prio-queue-insert
              end-perform

              *> prepare next iteration
              *> and empty the successors list for next iteration
              set d100-max-succ-idx to 1

              set d100-max-ready-idx up by 1 *> TODO catch overflow
              add 1 to d100-next-iteration 
              set d100-max-idx up by 1 *> TODO catch overflow, 
                                       *> but how when usin
              *> next loop, receive next node from priority queue
              perform q120-prio-queue-pop
           end-perform

           *> trace back for result
           *>   the result resides in the last row of dijkstra tab
           set d100-idx to d100-max-idx
           set d100-idx down by 1
           *>
      *    display "----RESULT----"
           set d100-max-path-idx to 1
           move 0 to d400-last-subs
           move d300-dijkstra-dest-subs to d100-subs
           perform test after 
                   until d100-subs = d300-dijkstra-start-subs
              if d400-last-subs not = 0
                 move d100-pred-subs(d100-idx d100-subs) to d100-subs
                 if edge-exists(d100-subs d400-last-subs)
                    set eidx to edge-idx(d100-subs d400-last-subs)
                    move graph-edge-weight(eidx)
                      to d100-res-dist(d100-path-idx)
                 end-if
              end-if
              move graph-node-id(d100-subs) 
                to d100-res-id(d100-max-path-idx)
              move graph-node-name(d100-subs)
                to d100-res-name(d100-max-path-idx)
              move d100-subs to d400-last-subs
              set d100-path-idx to d100-max-path-idx
              set d100-max-path-idx up by 1
              move d100-subs to d400-last-subs
           end-perform
           move 0
             to d100-res-dist(d100-path-idx)

           continue.
      ******************************************************************
      ******************************************************************
      *    write result
      ******************************************************************
       d500-write-result section.
           set d100-path-idx to d100-max-path-idx
           move 1 to d500-output-ptr
           perform TEST AFTER
                   until 1 = d100-path-idx
            set d100-path-idx down by 1
            move spaces to z400-in-string
            move d100-res-name(d100-path-idx) to z400-in-string
            perform z400-trim-string
            display d100-res-id(d100-path-idx)(1:4) ","
                    z400-in-string(z400-out-start-ptr: z400-out-len)
                    ","
                    d100-res-dist(d100-path-idx)
           end-perform
           continue.
      ******************************************************************
      ******************************************************************
      *    trace dijkstra table
      ******************************************************************
       d900-dijkstra-trace section.
           display "Dijkstra Trace"
           display "--------------"
           display "Start node subscript: " d300-dijkstra-start-subs 
           display "Dest node subscript: " d300-dijkstra-dest-subs 
           display "current node subscript: " d100-current-node-subs
           
           *> show queue
           perform q900-prio-queue-display

           *> show successor nodes
           set d100-succ-idx to 1
           move 1 to d900-output-ptr
           string "successors: " DELIMITED BY SIZE
             into d900-output
             WITH POINTER d900-output-ptr
           perform varying d100-succ-idx from 1 by 1
                     until d100-max-succ-idx <= d100-succ-idx
            string d100-succ-node-subs(d100-succ-idx) DELIMITED BY SIZE
                   "," DELIMITED BY SIZE 
              into d900-output
              WITH POINTER d900-output-ptr
      *     display d100-succ-node-subs(d100-succ-idx)
           end-perform
           SUBTRACT 2 from d900-output-ptr
           if d900-output-ptr > 0
              display d900-output(1:d900-output-ptr)
           else 
              display "No Successors"
           end-if

           *> ready nodes
           set d100-ready-idx to 1
           if d100-ready-idx = d100-max-ready-idx
              display "no ready nodes"
           else
              move 1 to d900-output-ptr
              string "ready: " DELIMITED BY SIZE 
                 into d900-output
                 WITH POINTER d900-output-ptr
              perform varying d100-ready-idx from 1 by 1
                        until d100-max-ready-idx <= d100-ready-idx
                string d100-ready-nodes(d100-ready-idx)
                       "," DELIMITED BY size
                  into d900-output
                  WITH POINTER d900-output-ptr
              end-perform
              SUBTRACT 2 from d900-output-ptr
              if d900-output-ptr > 0
                 display d900-output(1:d900-output-ptr)
              else 
                 display "No ready nodes"
              end-if
           end-if


           *> show dijkstra matrix
           move 0 to d900-step
           move 0 to d900-node-no
           INITIALIZE d900-output
           move 1 to d900-output-ptr
           string "Step" DELIMITED BY size
                  "|" delimited by size
             into d900-output WITH POINTER d900-output-ptr
           END-STRING 
           PERFORM VARYING nidx FROM 1 BY 1
                     UNTIL max-node-idx < nidx
            add 1 to d900-node-no
            move graph-node-name(d900-node-no) to d900-node-name
            string d900-node-name(1:4) delimited by size
                   "|" delimited by size
              into d900-output WITH POINTER d900-output-ptr
            end-string
           end-perform
           DISPLAY d900-output

           *> dashed line
           compute z900-line-len = d900-output-ptr - 1
           perform z900-display-hline

           *> distances line
           set d100-idx to 1
           PERFORM test BEFORE VARYING d100-idx FROM 1 BY 1 
                     UNTIL d100-max-idx <= d100-idx 
             *> distances
             INITIALIZE d900-output
             move 1 to d900-output-ptr
             add 1 to d900-step
             string d100-iteration(d100-idx) DELIMITED BY size
                    "|"                      DELIMITED BY SIZE
               into d900-output with POINTER d900-output-ptr
             end-string
             move 0 to d900-node-no
             PERFORM VARYING d100-node-idx FROM 1 BY 1
                       UNTIL d100-max-node-idx <= d100-node-idx
               move d100-dist(d100-idx d100-node-idx) 
                 to d900-out-dist
               string d900-out-dist DELIMITED BY size         
                      "|"           delimited by size
                 into d900-output WITH POINTER d900-output-ptr
             end-perform
             subtract 1 from d900-output-ptr
             display d900-output(1:d900-output-ptr)
             *> predecessors
             INITIALIZE d900-output
             move 1 to d900-output-ptr
             add 1 to d900-step
             string "    |" DELIMITED BY SIZE
               into d900-output with POINTER d900-output-ptr
             end-string
             move 0 to d900-node-no
             PERFORM VARYING d100-node-idx FROM 1 BY 1
                       UNTIL d100-max-node-idx <= d100-node-idx
               move d100-pred-subs(d100-idx d100-node-idx) to d900-subs
               move graph-node-name(d900-subs)
                 to d900-node-name
               *>string d900-subs DELIMITED BY size         
               string d900-node-name(1:4) DELIMITED BY size         
                      "|"       delimited by size
                 into d900-output WITH POINTER d900-output-ptr
             end-perform
             subtract 1 from d900-output-ptr
             display d900-output(1:d900-output-ptr)
             perform z900-display-hline
           end-perform

           display "End of Dijkstra Trace"

           continue.
      ******************************************************************

      ******************************************************************
      *    read graph csv
      ******************************************************************
       c100-parse-csv-line section.
           *> trim the input line
           move c100-in-str to z400-in-string
           perform z420-right-trim-string
           move z400-out-start-ptr to c100-instr-ptr

           *> split
           set c100-cidx to 1
           set c100-max-cidx to 1
           move 1 to c100-i
           if z400-out-len > 0
              perform until c100-i > z400-out-len 
                move 0 to c100-l
                *> remember start pos of column
                move c100-i to c100-out-start-ptr(c100-cidx)

                *> search next , position
                perform until c100-in-str(c100-instr-ptr:1) = ','
                           or c100-i > z400-out-len
                  add 1 to c100-l         *> increse len 
                  add 1 to c100-instr-ptr *> move foreward in raw line
                  add 1 to c100-i         *> move foreward in trimmed line
                end-perform
                *> overread separator
                add 1 to c100-instr-ptr  *> move foreward in raw line
                add 1 to c100-i          *> move foreward in trimmed line

                *> remember len of column
                move c100-l to c100-out-len(c100-cidx)

                *> next column
                set c100-max-cidx to c100-cidx
                set c100-cidx up by 1
              end-perform
           end-if

           continue.
      ******************************************************************
      *    priority queue
      ******************************************************************
       q100-prio-queue-init section.
           perform t100-bt-init
           continue.
       q110-prio-queue-insert section.
           *> in: q110-new-entry
           *> prog-status: 0, err02-queue-full
           *>
           move q110-new-entry to t110-bt-in-parms
           perform t110-bt-insert 
           continue.
       q120-prio-queue-pop section.
           *> out: q120-next-entry
           *> out: bool: q120-queue-empty
           *> 
           perform t130-bt-pop-min
           if t130-bt-pop-min-res-ok
              move t130-bt-pop-min-result-data 
                to q120-next-entry 
              set q120-result-ok to true
           else
              set q120-result-q-empty to true
           end-if
           continue.
       q900-prio-queue-display section.
           display "Priority queue:"
           perform t910-bt-display-bredth-firts
           continue.
      ******************************************************************

      ******************************************************************
      *    binary heap
      ******************************************************************
       t100-bt-init section.
           move 0 to t100-root
           move 1 to t100-next-free-pos 
           continue.
       t110-bt-insert section.
           if t100-next-free-pos <= 999
              move t100-root to t110-cur-nd
              move t100-root to t110-last-nd
              *> search the approlriate branch
              set t110-do-insert to true
              perform until t110-cur-nd = 0
                        or t110-dont-insert
                 if t100-entry-id(t110-cur-nd) not = t110-entry-id    
                    move t110-cur-nd to t110-last-nd
                    if t110-entry-weight 
                     < t100-entry-weight(t110-cur-nd)
                       *> go left
                       move t100-bt-left(t110-cur-nd)  to t110-cur-nd 
                    else
                       *> go right
                       move t100-bt-right(t110-cur-nd) to t110-cur-nd 
                    end-if
                 else
      *             display "nothing to insert"
      *             display "  new entry " t110-entry-id
      *             display "  is already in tree at pos " t110-cur-nd
      *                     "  with value " t100-entry-id(t110-cur-nd)
                    set t110-dont-insert to true
                 end-if
              end-perform
              if t110-do-insert
                 *> save in tree at next free position
                 move t110-bt-in-parms 
                   to t100-bt-entry(t100-next-free-pos)
                 move 0 to t100-bt-left(t100-next-free-pos)
                 move 0 to t100-bt-right(t100-next-free-pos)
                 if t100-root = 0 
                    *> it's the root
      *             display "insert root " t110-bt-in-parms  
                    move t100-next-free-pos to t100-root
                 else
                    *> connect with branch
                    if t110-entry-weight 
                       < t100-entry-weight(t110-last-nd)
                       *> insert left
      *                display "insert left " t110-bt-in-parms  
                       move t100-next-free-pos 
                         to t100-bt-left(t110-last-nd)
                    else
                       *> insert right
      *                display "insert right " t110-bt-in-parms  
                       move t100-next-free-pos 
                         to t100-bt-right(t110-last-nd)
                    end-if
                 end-if
                 add 1 to t100-next-free-pos
              end-if
           else
              display "ERROR: no space left for new tree node"
              move :err-tree-full: to prog-status
           end-if
           continue.
      *t120-bt-del-elem section.
      *    display "001" t120-del-entry-id 
      *    set t120-result-ok to true
      *    *> find position of the
      *    move t100-root to t120-cur-pos
      *    move 0 to t120-found-pos
      *    perform until t120-cur-pos = 0
      *               or t120-found-pos not = 0
      *       if t100-entry-weight(t120-cur-pos) = t120-del-entry-id
      *          move t120-cur-pos to t120-found-pos
      *       else
      *         if t100-entry-weight(t120-cur-pos) < t120-del-entry-id
      *           move t100-bt-right(t120-cur-pos) to t120-cur-pos
      *         else
      *           move t100-bt-left(t120-cur-pos) to t120-cur-pos
      *         end-if 
      *       end-if
      *    end-perform
      *    display "002" t120-cur-pos 
      *    if t120-found-pos not = 0
      *       *> TODO
      *       set t120-result-ok to true
      *    else
      *       set t120-result-not-found to true
      *    end-if
      *    continue.

       t130-bt-pop-min section.
           set t130-bt-pop-min-res-ok to true
           *> find position of the
           move t100-root to t130-cur-pos
           move 0 to t130-found-pos
           move 0 to t130-prev-pos
           perform until t130-cur-pos = 0
                      or t130-found-pos not = 0
             if t100-bt-left(t130-cur-pos) = 0
              *> nothing more left than least elem found
              move t130-cur-pos to t130-found-pos
             else 
               move t130-cur-pos to t130-prev-pos
               move t100-bt-left(t130-cur-pos) to t130-cur-pos
             end-if 
           end-perform
      *    if t130-found-pos > 0
      *       display "min pos: " t130-found-pos 
      *               " / " t100-bt(t130-found-pos)
      *    else
      *       display "not found min pos: " t130-found-pos 
      *    end-if
           if t130-found-pos not = 0
             if t130-found-pos = t100-root
      *         display "root is min"
                move t100-bt-right(t130-found-pos) to t100-root
             else 
      *         display "adjust left from prev pos " t130-prev-pos 
      *                 " / " t100-bt(t130-prev-pos)
                if t100-bt-left(t130-prev-pos) = t130-found-pos
      *           display "assertion "
      *                   "t100-bt-left(t130-prev-pos) = t130-found-pos"
      *                   " met"
                  continue
                else
      d           display "failed to meet assertion "
      d                   "t100-bt-left(t130-prev-pos) = t130-found-pos"
                end-if
                move t100-bt-right(t130-found-pos) 
                  to t100-bt-left(t130-prev-pos)
             end-if
             move t100-bt(t130-found-pos) to t130-bt-pop-min-result-data
             set t130-bt-pop-min-res-ok to true
           else
              set t130-bt-pop-min-res-not-found to true
           end-if
           continue.
       t910-bt-display-bredth-firts section.
           display "Binary Tree bredth first traversal"
           if t100-root not = 0
              perform s100-stack-init
              move t100-root to t910-pos 
              perform s130-stack-check-empty
              perform test after until s130-stack-empty 
                                   and t910-pos = 0
                 *> traverse to the left (smaller elements)
                 perform until t910-pos = 0
                    move t910-pos to s110-elem
                    perform s110-stack-push
                    move t100-bt-left(t910-pos) to t910-pos
                 end-perform
                 *> visit node
                 perform s120-stack-pop
                 move s120-elem to t910-pos
                 display t100-entry-id(t910-pos)
                         ":"
                         t100-entry-weight(t910-pos)
                 *> continue with right branch
                 if t100-bt-right(t910-pos) not = 0
                    move t100-bt-right(t910-pos) to t910-pos
                 else
                    move 0 to t910-pos
                 end-if
                 perform s130-stack-check-empty
              end-perform
           end-if   
           display "End of Binary Tree bredth first traversal"
           continue.
       t990-bt-display-raw section.
           display "Binary Tree flat"
           move 1 to t990-pos
           perform until t100-next-free-pos <= t990-pos
              display t100-bt(t990-pos)
              add 1 to t990-pos
           end-perform
           display "End of Binary Tree flat"
           continue.
      ******************************************************************

      ******************************************************************
      *    stack
      ******************************************************************
       s100-stack-init section.
           set s100-top-idx to 1
           set s100-bottom-idx to 1
           set s130-stack-empty to true
           continue.
       s110-stack-push section.
           if s100-top-idx < 999
              move s110-elem to s100-stack-elem(s100-top-idx) 
              set s100-top-idx up by 1
           else
      d       display "ERROR: Stack overflow."
              move :err-stack-overflow: to prog-status
           end-if
           continue.
       s120-stack-pop section.
           if s100-bottom-idx = s100-top-idx 
              display "ERROR: access to empty stack."
              move :err-empty-stack: to prog-status
           else
              set s100-top-idx down by 1
              move s100-stack-elem(s100-top-idx) to s120-elem
           end-if
           continue.
       s130-stack-check-empty section.
           if s100-bottom-idx = s100-top-idx 
              set s130-stack-empty to true
           else
              set s130-stack-not-empty to true
           end-if 
           continue.
      ******************************************************************

      ******************************************************************
      *    random
      ******************************************************************
       r100-random-init section.
           perform r105-seed-random 
           continue.

       r105-seed-random SECTION .
           move FUNCTION CURRENT-DATE to curdate
           compute frnd = Function RANDOM (curdate-num-part)
           continue.

       r110-gen-irnd section.
           compute frnd = function RANDOM
           move frnd(4::maxlen:) to r110-ret-irnd.
           continue.

      ******************************************************************
      
      ******************************************************************
      *    helpers
      ******************************************************************
       z100-search-node-by-name-proc section.
           set nidx to 1
           move 0 to z100-res-node-subscript
           move 0 to z100-i
           search graph-nodes VARYING z100-i
              at end
                 move :err01-invalid-node-name: to prog-status
              when graph-node-name(nidx) = z100-search-nd-nme 
                 move graph-node-id(nidx) to z100-res-node-id
                 compute z100-res-node-subscript = z100-i + 1
                 set z100-res-node-idx to nidx
           end-search
           continue.
       z200-search-node-by-id-proc section.
           set nidx to 1
           move 0 to z200-res-node-subscript
           move 0 to z200-i
           search graph-nodes VARYING z100-i
              at end
                 move :err-invalid-node-id: to prog-status
              when graph-node-id(nidx) = z200-search-nd-id 
                 compute z200-res-node-subscript = z200-i + 1
                 set z200-res-node-idx to nidx
           end-search
           continue.
       z300-display-adj-matrix-proc section.
           display "Adjacence Matrix:"
           *> header line
           move 1 to z300-ptr
           string "    |" DELIMITED BY size
             into z300-output
             WITH POINTER z300-ptr
           END-STRING
           perform VARYING to-i from 1 by 1 until num-nodes < to-i
              move graph-node-name(to-i)(1:4) to  z300-name
              string z300-name delimited by size
                     "|"       DELIMITED BY SIZE 
                into z300-output
                WITH POINTER z300-ptr
           end-perform
           subtract 1 from z300-ptr GIVING z300-hline-len
           display z300-output(1:z300-hline-len)
           move z300-hline-len to z900-line-len IN z900-interface-vars
           perform z900-display-hline

           *> matrix
           perform VARYING from-i from 1 by 1 until num-nodes < from-i
             move 1 to z300-ptr
             move graph-node-name(from-i)(1:4) to  z300-name
             string z300-name delimited by size
                    "|"       DELIMITED BY SIZE 
               into z300-output
               WITH POINTER z300-ptr
             perform VARYING to-i from 1 by 1 until num-nodes < to-i
                if edge-exists(from-i to-i)
                 set eidx to edge-idx(from-i to-i)
                 move graph-edge-weight(eidx) to z300-weight
                 string z300-weight DELIMITED BY SIZE
                        "|"         DELIMITED BY SIZE
                   into z300-output
                   WITH POINTER z300-ptr
                else
                 string " -- |" DELIMITED BY SIZE
                   into z300-output
                   WITH POINTER z300-ptr
                end-if
             end-perform
             display z300-output(1:z300-hline-len)
             move z300-hline-len to z900-line-len IN z900-interface-vars
             perform z900-display-hline
           end-perform
           *> footer
           perform VARYING from-i from 1 by 1 until num-nodes < from-i
             perform VARYING to-i from 1 by 1 until num-nodes < to-i
              if edge-exists( from-i to-i) then
                 set eidx to edge-idx(from-i to-i)
                 display "(" from-i "," to-i ") = "
                         graph-edge-weight(eidx)
              else
                 display "(" from-i "," to-i ") = nil"
              end-if 
             end-perform
           end-perform
           continue.
       z400-trim-string section.
           *> in:
           *>      z400-in-string
           *> out:
           *>      z400-out-start-ptr
           *>      z400-out-len
           perform z410-left-trim-string
           if z400-out-start-ptr > z400-total-len 
              *> only spaces found => empty string
              move 1 to z400-out-start-ptr
              move 0 to z400-out-len 
           else
              perform z421-right-trim-string
           end-if
           continue.
       z410-left-trim-string section.
           move LENGTH OF z400-in-string to z400-total-len
           move 1 to z400-out-start-ptr
           perform varying z400-out-start-ptr from 1 by 1
                     until z400-out-start-ptr > z400-total-len
                     or z400-in-string(z400-out-start-ptr:1) not = space
           end-perform
           if z400-out-start-ptr > z400-total-len 
             *> only spaces found => empty string
             move 1 to z400-out-start-ptr
             move 0 to z400-out-len 
           else
             compute z400-out-len 
                   = z400-total-len - z400-out-start-ptr + 1
           end-if

           continue.
       z420-right-trim-string section.
           move 1 to z400-out-start-ptr
           perform z421-right-trim-string
           continue.

       z421-right-trim-string section.
           move LENGTH OF z400-in-string to z400-total-len
           perform varying z400-left-trim-pos 
                      from z400-total-len by -1
                     until z400-in-string(z400-left-trim-pos:1)
                           not = space
                        or z400-left-trim-pos < z400-out-start-ptr
           end-perform
           if z400-left-trim-pos < z400-out-start-ptr 
              *> only spaces found => empty string
              move 1 to z400-out-start-ptr
              move 0 to z400-out-len 
           else
              compute z400-out-len =
                      z400-left-trim-pos - z400-out-start-ptr + 1
              *>display "start " z400-out-start-ptr
              *>        " end " z400-left-trim-pos
              *>        " len " z400-out-len
              *>        " str |" z400-in-string(z400-out-start-ptr:
              *>                              z400-out-len)
              *>        "|"
           end-if

           continue.

       z900-display-hline section.
           move 0 to z900-i
           perform z900-line-len TIMES 
              add 1 to z900-i
              move "-" to z900-output(z900-i:1)
           end-perform
           display z900-output(1:z900-line-len)
           continue.
      ******************************************************************
       End program DIJK.

theory system_model
  imports Main
    ZFC_utils
    notations
begin


text \<open>We use natural numbers to identify nodes in the system.\<close>

datatype nodeId = NodeId nat



text \<open>For modelling cryptography, i.e. signing of messages, we introduce 
a type class that extracts the messages from a value.

Messages use the type V from ZFC because type classes can only have one type
parameter in Isabelle.
\<close>

datatype signedMessage = SignedMessage (message_sender: nodeId) (message_content: V)

class value_class = 
  fixes messages_in_value :: \<open>'a \<Rightarrow> signedMessage set\<close>





text \<open>A node in the system has an inbox of messages, a state, and  \<close>

record ('state, 'message) node = 
  node_state :: 'state
  node_inbox :: \<open>'message list\<close>
  node_knownSigned :: \<open>signedMessage set\<close>


record ('state, 'message) system =
  system_nodes :: \<open>nodeId \<rightharpoonup> ('state, 'message) node\<close>
  faulty_nodes :: \<open>nodeId set\<close>



record ('state, 'message) prog = 
  prog_on_receive :: \<open>nodeId \<Rightarrow> 'state \<Rightarrow> 'message \<Rightarrow> ('state \<times> ((nodeId \<times> 'message) list))\<close>
  

datatype vote = Vote nat

datatype ('state, 'message) node_action = 
    ReceiveMessage 'message
  | FaultyReceiveMessage 'message 'state \<open>(nodeId \<times> 'message) list\<close> 
  | VoteAction vote 

datatype ('state, 'message) action = Action (action_nodeId:nodeId) \<open>('state, 'message) node_action\<close>


type_synonym ('state, 'message) trace = \<open>('state, 'message) action list\<close>

inductive step :: \<open>('state, 'message) prog \<Rightarrow> ('state, 'message) action \<Rightarrow> ('state, 'message) system \<Rightarrow> ('state, 'message) system \<Rightarrow> bool\<close> for prog
  where
step_ReceiveMessage: 
\<open>\<lbrakk>system_nodes S n \<triangleq> Sn;
msg \<in> set (node_inbox Sn);
prog_on_receive prog n (node_state Sn) msg = (newNodeState, newMessages);
S' = S\<lparr>
  system_nodes := Sn'
\<rparr>
\<rbrakk> \<Longrightarrow> step prog (Action n (ReceiveMessage msg)) S S'\<close>








end
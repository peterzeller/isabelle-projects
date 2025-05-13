theory system_model
  imports Main

begin

type_synonym node = nat
type_synonym client = nat


text \<open>A message can be a natural number (all finite datatypes can be encoded as nat) 
or a signature over a message or a sequence of messages.\<close>

datatype message =
    NatMessage nat
  | SignedMessage node message
  | SeqMessage \<open>message list\<close>


type_synonym node_state = message

fun signed_messages :: \<open>message \<Rightarrow> (node \<times> message) set\<close> where
  \<open>signed_messages (NatMessage _) = {}\<close>
| \<open>signed_messages (SignedMessage n m) = {(n,m)} \<union> signed_messages m\<close>
| \<open>signed_messages (SeqMessage xs) = (\<Union>x\<in>set xs. signed_messages x)\<close>


text \<open>An actor can perform multiple actions in one step which are described by the following record:\<close>

record action =
  client_response :: \<open>(client \<times> message) option\<close>
  network_messages :: \<open>(node \<times> message) set\<close>
  new_state :: \<open>node_state\<close>

find_consts \<open>'a option \<Rightarrow> 'a set\<close>

definition 
\<open>messages_action a \<equiv> 
  snd ` set_option (client_response a)
  \<union> snd ` network_messages a
  \<union> {new_state a}\<close>

definition signed_messages_action where
\<open>signed_messages_action a \<equiv>
  \<Union>(signed_messages ` messages_action a)\<close>


record system_state = 
  nodes :: \<open>node set\<close>
  states :: \<open>node \<rightharpoonup> message\<close>
  network :: \<open>(node \<times> (node \<times> message)) set\<close>
  known_signatures :: \<open>node \<rightharpoonup> ((node \<times> message) set)\<close>


record algorithm =
  init_state :: \<open>node \<Rightarrow> node_state\<close>
  process_client_message :: \<open>node_state \<Rightarrow> client \<Rightarrow> message \<Rightarrow> action\<close>
  process_network_message :: \<open>node_state \<Rightarrow> node \<Rightarrow> message \<Rightarrow> action\<close>



definition algorithm_well_formed where
\<open>algorithm_well_formed A \<equiv> 
  (\<forall>n n' m.  (n', m) \<in> signed_messages (init_state A n) \<longrightarrow> n' = n)
 \<and> (\<forall>s c m. signed_messages_action (process_client_message A s c m) \<subseteq> signed_messages s)
 \<and> (\<forall>s n m. signed_messages_action (process_client_message A s c m) \<subseteq> signed_messages s)\<close>





text \<open>The system can do the following steps:\<close>

datatype step =
    ReceiveClientMessage node message
  | ReceiveInternal node node message






end
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

type_synonym client_response = \<open>(client \<times> message)\<close>

record action =
  client_response :: \<open> client_response option\<close>
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
  node_state :: \<open>node \<rightharpoonup> message\<close>
\<comment> \<open>network consists of set of triples: sender \<times> recipient \<times> message\<close>
  network :: \<open>(node \<times> (node \<times> message)) set\<close>
  known_signatures :: \<open>node \<rightharpoonup> ((node \<times> message) set)\<close>


record algorithm =
  init_state :: \<open>node \<Rightarrow> node_state\<close>
  process_client_message :: \<open>node_state \<Rightarrow> client \<Rightarrow> message \<Rightarrow> action\<close>
\<comment> \<open>process a message (state, sender, message)\<close>
  process_network_message :: \<open>node_state \<Rightarrow> node \<Rightarrow> message \<Rightarrow> action\<close>



definition algorithm_well_formed where
\<open>algorithm_well_formed A \<equiv> 
  (\<forall>n n' m.  (n', m) \<in> signed_messages (init_state A n) \<longrightarrow> n' = n)
 \<and> (\<forall>s c m. signed_messages_action (process_client_message A s c m) \<subseteq> signed_messages s \<union> signed_messages m)
 \<and> (\<forall>s n m. signed_messages_action (process_network_message A s n m) \<subseteq> signed_messages s \<union> signed_messages m )\<close>





text \<open>A node can do the following steps:\<close>

datatype step =
    ReceiveClientMessage client node message
  | ReceiveInternal node node message

definition 
\<open>execute_action S n a \<equiv> 
(client_response a,
  S\<lparr>
    node_state := (node_state S)(n \<mapsto> new_state a),
    network := {n} \<times> network_messages a
 \<rparr>)\<close>

definition execute_step :: \<open>algorithm \<Rightarrow> system_state \<Rightarrow> step \<Rightarrow> client_response option \<times> system_state\<close> where
\<open>execute_step A S step \<equiv> case step of
  ReceiveClientMessage c n msg \<Rightarrow> 
    (case node_state S n of
        None \<Rightarrow> (None, S)
      | Some ns \<Rightarrow>
          let a = process_client_message A ns c msg  in
          execute_action S n a)
| ReceiveInternal sender recipient msg \<Rightarrow>
  if (sender, recipient, msg) \<in> network S then
    (case node_state S recipient of
        None \<Rightarrow> (None, S)
      | Some ns \<Rightarrow>
    let a = process_network_message A ns sender msg in
    execute_action S recipient a)
  else
    (None, S)\<close>





end
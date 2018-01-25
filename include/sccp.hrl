%%% sccp.hrl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2015-2018 SigScale Global Inc.
%%% @end
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%% SCCP message type - ITU-T Recommendation Q.713, section 2.1
-define(ConnectRequest,             1).
-define(ConnectionConfirm,          2).
-define(ConnectionRefused,          3).
-define(Released,                   4).
-define(ReleaseComplete,            5).
-define(DataForm1,                  6).
-define(DataForm2,                  7).
-define(DataAck,                    8).
-define(UnitData,                   9).
-define(UnitDataService,           10).
-define(ExpeditedData,             11).
-define(ExpeditedDataAck,          12).
-define(ResetRequest,              13).
-define(ResetConfirmation,         14).
-define(ProtocolDataUnitError,     15).
-define(InactivityTest,            16).
-define(ExtendedUnitData,          17).
-define(ExtendedUnitDataService,   18).
-define(LongUnitData,              19).
-define(LongUnitDataService,       20).

%% SCCP parameters - ITU-T Recommendation Q.713, section 3
-define(ENDOFOPTIONALPARAMETERS,    0).
-define(DESTINATIONLOCALREFERENCE,  1).
-define(SOURCELOCALREFERENCE,       2).
-define(CALLEDPARTYADDRESS,         3).
-define(CALLINGPARTYADDRESS,        4).
-define(PROTOCOLCLASS,              5).
-define(SEGMENTING,                 6).
-define(RECEIVESEQUENCENUMBER,      7).
-define(SEQUENCING,                 8).
-define(CREDIT,                     9).
-define(RELEASECAUSE,              10).
-define(RETURNCAUSE,               11).
-define(RESETCAUSE,                12).
-define(ERRORCAUSE,                13).
-define(REFUSALCAUSE,              14).
-define(DATA,                      15).
-define(SEGMENTATION,              16).
-define(HOPCOUNTER,                17).
-define(IMPORTANCE,                18).
-define(LONGDATA,                  19).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.4, section 3.5
-record(party_address,
		{ri :: boolean(),
		pc :: pos_integer(),
		ssn :: pos_integer(),
		translation_type :: not_used | internetwork | network_specific | reserved,
		numbering_plan :: unknown | isdn_tele | generic
				| data | telex | maritime | land_mobile
				| isdn_mobile | spare | private_net | reserved,
		encoding_scheme :: unknown | bcd_odd | bcd_even
				| national | spare | reserved,
		nai :: unknown | subscriber | national | international | spare | reserved,
		gt :: [integer()]}).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.17
-record(segmentation,
		{first :: boolean(),
		class :: 0..1,
		remaning_seg :: 0..15}).

%% SCCP messages - ITU-T Recommendation Q.713, section 4
-record(sccp_connection_req,
		{type = ?ConnectRequest,
		src_local_ref :: pos_integer(),
		class :: pos_integer(),
		called_party :: #party_address{},
		credit :: binary(),
		calling_party :: #party_address{},
		data :: binary(),
		hop_counter :: pos_integer(),
		importance :: pos_integer()}).

-record(sccp_connection_confirm,
		{type  = ?ConnectionConfirm,
		dest_local_ref :: pos_integer(),
		src_local_ref :: pos_integer(),
		class :: pos_integer(),
		credit :: binary(),
		called_party :: #party_address{},
		data :: binary(),
		importance :: pos_integer()}).

-record(sccp_connection_refused,
		{type = ?ConnectionRefused,
		dest_local_ref :: pos_integer(),
		refusal_cause :: enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | dest_unknown | dest_inaccessible | network_resource
				| access_failure | access_congestion | subsystem_failure
				| subsystem_congestion | connection_expire | incomp_userdata
				| reserved | unqualified | hcounter_violation | sccp_failure
				| no_translation_addr | unequipped_error,
		called_party :: #party_address{},
		data :: binary(),
		importance :: pos_integer()}).

-record(sccp_released,
		{type = ?Released,
		dest_local_ref :: pos_integer(),
		src_local_ref :: pos_integer(),
		release_cause :: enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | rpc_error | inconsistant_data | access_failure
				| access_congestion | subsystem_failure | subsystem_congestion
				| mtp_failure | network_congestion | timer_expire | reserved
				| unqualified | sccp_failure,
		data :: binary(),
		importance :: pos_integer()}).

-record(sccp_release_complete,
		{type = ?ReleaseComplete,
		dest_local_ref :: pos_integer(),
		src_local_ref :: pos_integer()}).

-record(sccp_data_form1,
		{type = ?DataForm1,
		dest_local_ref :: pos_integer(),
		segmenting :: boolean(),
		data :: binary()}).

-record(sccp_data_form2,
		{type = ?DataForm2,
		dest_local_ref :: pos_integer(),
		sequencing :: boolean(),
		data :: binary()}).

-record(sccp_data_ack,
		{type = ?DataAck,
		dest_local_ref :: pos_integer(),
		receive_seq_num :: binary(),
		credit :: binary()}).

-record(sccp_unitdata,
		{type = ?UnitData,
		class :: pos_integer(),
		called_party :: #party_address{},
		calling_party :: #party_address{},
		data :: binary()}).

-record(sccp_unitdata_service,
		{type = ?UnitDataService,
		return_cause :: no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		called_party :: #party_address{},
		calling_party :: #party_address{},
		data :: binary()}).

-record(sccp_expedited_data,
		{type = ?ExpeditedData,
		dest_local_ref :: pos_integer(),
		data :: binary()}).

-record(sccp_expedited_ack,
		{type = ?ExpeditedDataAck,
		 dest_local_ref :: pos_integer()}).

-record(sccp_reset_request,
		{type = ?ResetRequest,
		dest_local_ref :: pos_integer(),
		src_local_ref :: pos_integer(),
		reset_cause}).

-record(sccp_reset_confirmation,
		{type = ?ResetConfirmation,
		dest_local_ref :: pos_integer(),
		src_local_ref :: pos_integer()}).

-record(sccp_protocol_data_unit_error,
		{type = ?ProtocolDataUnitError,
		dest_local_ref :: pos_integer(),
		error_cause}).
		
-record(sccp_inactivity_test,
		{type = ?InactivityTest,
		dest_local_ref :: pos_integer(),
		src_local_ref :: pos_integer(),
		class :: pos_integer(),
		sequencing :: boolean(),
		credit :: binary()}).

-record(sccp_extended_unitdata,
		{type = ?ExtendedUnitData,
		class :: pos_integer(),
		hop_counter :: pos_integer(),
		called_party :: #party_address{},
		calling_party :: #party_address{},
		data :: binary(),
		segmentation :: #segmentation{},
		importance :: pos_integer()}).

-record(sccp_extended_unitdata_service,
		{type = ?ExtendedUnitDataService,
		return_cause :: no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		hop_counter :: pos_integer(),
		called_party :: #party_address{},
		calling_party :: #party_address{},
		data :: binary(),
		segmentation :: #segmentation{},
		importance :: pos_integer()}).

-record(sccp_long_unitdata,
		{type = ?LongUnitData,
		class :: pos_integer(),
		hop_counter :: pos_integer(),
		called_party :: #party_address{},
		calling_party :: #party_address{},
		long_data :: binary(),
		segmentation :: #segmentation{},
		importance :: pos_integer()}).

-record(sccp_long_unitdata_service,
		{type = ?LongUnitDataService,
		return_cause :: no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		hop_counter :: pos_integer(),
		called_party :: #party_address{},
		calling_party :: #party_address{},
		long_data :: binary(),
		segmentation :: #segmentation{},
		importance :: pos_integer()}).


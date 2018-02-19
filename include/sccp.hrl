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
-define(EndOfOptionalParameters,    0).
-define(DestinationLocalRef,        1).
-define(SourceLocalRef,             2).
-define(CalledPartyAddress,         3).
-define(CallingPartyAddress,        4).
-define(ProtocolClass,              5).
-define(Segmenting,                 6).
-define(ReceiveSequenceNum,         7).
-define(Sequencing,                 8).
-define(Credit,                     9).
-define(ReleaseCause,              10).
-define(ReturnCause,               11).
-define(ResetCause,                12).
-define(ErrorCause,                13).
-define(RefusalCause,              14).
-define(DATA,                      15).
-define(Segmentation,              16).
-define(HopCounter,                17).
-define(Importance,                18).
-define(LongData,                  19).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.4, section 3.5
-record(party_address,
		{ri :: undefined | boolean(),
		pc :: undefined | pos_integer(),
		ssn :: undefined | pos_integer(),
		translation_type :: undefined | not_used | internetwork | network_specific | reserved,
		numbering_plan :: undefined | unknown | isdn_tele | generic
				| data | telex | maritime | land_mobile
				| isdn_mobile | spare | private_net | reserved,
		encoding_scheme :: undefined | unknown | bcd_odd | bcd_even
				| national | spare | reserved,
		nai :: undefined | unknown | subscriber | national | international | spare | reserved,
		gt :: undefined | [integer()]}).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.17
-record(segmentation,
		{first :: undefined | boolean(),
		class :: undefined | 0..1,
		remaning_seg :: undefined | 0..15}).

%% SCCP messages - ITU-T Recommendation Q.713, section 4
-record(sccp_connection_req,
		{type = ?ConnectRequest,
		src_local_ref :: undefined | pos_integer(),
		class :: undefined | pos_integer(),
		called_party :: undefined | #party_address{},
		credit :: undefined | binary(),
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		hop_counter :: undefined | pos_integer(),
		importance :: undefined | pos_integer()}).

-record(sccp_connection_confirm,
		{type  = ?ConnectionConfirm,
		dest_local_ref :: undefined | pos_integer(),
		src_local_ref :: undefined | pos_integer(),
		class :: pos_integer(),
		credit :: undefined | binary(),
		called_party :: undefined | #party_address{},
		data :: undefined | binary(),
		importance :: undefined | pos_integer()}).

-record(sccp_connection_refused,
		{type = ?ConnectionRefused,
		dest_local_ref :: undefined | pos_integer(),
		refusal_cause :: undefined | enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | dest_unknown | dest_inaccessible | network_resource
				| access_failure | access_congestion | subsystem_failure
				| subsystem_congestion | connection_expire | incomp_userdata
				| reserved | unqualified | hcounter_violation | sccp_failure
				| no_translation_addr | unequipped_error,
		called_party :: undefined | #party_address{},
		data :: undefined | binary(),
		importance :: undefined | pos_integer()}).

-record(sccp_released,
		{type = ?Released,
		dest_local_ref :: undefined | pos_integer(),
		src_local_ref :: undefined | pos_integer(),
		release_cause :: undefined | enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | rpc_error | inconsistant_data | access_failure
				| access_congestion | subsystem_failure | subsystem_congestion
				| mtp_failure | network_congestion | timer_expire | reserved
				| unqualified | sccp_failure,
		data :: undefined | binary(),
		importance :: undefined | pos_integer()}).

-record(sccp_release_complete,
		{type = ?ReleaseComplete,
		dest_local_ref :: undefined | pos_integer(),
		src_local_ref :: undefined | pos_integer()}).

-record(sccp_data_form1,
		{type = ?DataForm1,
		 dest_local_ref :: undefined | pos_integer(),
		segmenting :: undefined | boolean(),
		data :: undefined | binary()}).

-record(sccp_data_form2,
		{type = ?DataForm2,
		dest_local_ref :: undefined | pos_integer(),
		sequencing :: undefined | boolean(),
		data :: undefined | binary()}).

-record(sccp_data_ack,
		{type = ?DataAck,
		dest_local_ref :: undefined | pos_integer(),
		receive_seq_num :: undefined | binary(),
		credit :: undefined | binary()}).

-record(sccp_unitdata,
		{type = ?UnitData,
		class :: undefined | pos_integer(),
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary()}).

-record(sccp_unitdata_service,
		{type = ?UnitDataService,
		return_cause :: undefined | no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary()}).

-record(sccp_expedited_data,
		{type = ?ExpeditedData,
		dest_local_ref :: undefined | pos_integer(),
		data :: undefined | binary()}).

-record(sccp_expedited_ack,
		{type = ?ExpeditedDataAck,
		 dest_local_ref :: undefined | pos_integer()}).

-record(sccp_reset_request,
		{type = ?ResetRequest,
		dest_local_ref :: undefined | pos_integer(),
		src_local_ref :: undefined | pos_integer(),
		reset_cause :: undefined | enduser_orig, sccp_user_oirg, incorrect_ps,
				incorrect_pr, rpc_error, remote_end_user_operational,
				network_operational, access_opertional, network_congestion,
				reserved, unqualified}).

-record(sccp_reset_confirmation,
		{type = ?ResetConfirmation,
		dest_local_ref :: undefined | pos_integer(),
		src_local_ref :: undefined | pos_integer()}).

-record(sccp_protocol_data_unit_error,
		{type = ?ProtocolDataUnitError,
		dest_local_ref :: undefined | pos_integer(),
		error_cause :: undefined | integer()}).
		
-record(sccp_inactivity_test,
		{type = ?InactivityTest,
		dest_local_ref :: undefined | pos_integer(),
		src_local_ref :: undefined | pos_integer(),
		class :: undefined | pos_integer(),
		sequencing :: undefined | boolean(),
		credit :: undefined | binary()}).

-record(sccp_extended_unitdata,
		{type = ?ExtendedUnitData,
		class :: undefined | pos_integer(),
		hop_counter :: undefined | pos_integer(),
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | pos_integer()}).

-record(sccp_extended_unitdata_service,
		{type = ?ExtendedUnitDataService,
		return_cause :: undefined | no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		hop_counter :: undefined | pos_integer(),
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | pos_integer()}).

-record(sccp_long_unitdata,
		{type = ?LongUnitData,
		class :: undefined | pos_integer(),
		hop_counter :: undefined | pos_integer(),
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		long_data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | pos_integer()}).

-record(sccp_long_unitdata_service,
		{type = ?LongUnitDataService,
		return_cause :: undefined | no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		hop_counter :: undefined | pos_integer(),
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		long_data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | pos_integer()}).


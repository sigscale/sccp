%%% sccp.hrl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2015-2021 SigScale Global Inc.
%%% @author Vance Shipley <vances@sigscale.org> [http://www.sigscale.org]
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
-define(Data,                      15).
-define(Segmentation,              16).
-define(HopCounter,                17).
-define(Importance,                18).
-define(LongData,                  19).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.4, section 3.5
-record(party_address,
		{ri = false :: boolean(),
		pc :: undefined | 1..16383,
		ssn :: undefined | 1..254,
		translation_type :: undefined | 0..254,
		numbering_plan :: undefined | unknown | isdn_tele | generic
				| data | telex | maritime | land_mobile
				| isdn_mobile | spare | private_net | reserved,
		encoding_scheme :: undefined | unknown | bcd_odd | bcd_even
				| national | spare | reserved,
		nai :: undefined | unknown | subscriber | national | international | spare | reserved,
		gt :: undefined | [byte()]}).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.9
-record(sequencing,
		{send_seq_num :: 0..127,
		receive_seq_num :: 0..127,
		more_data :: boolean()}).

%% SCCP Parameters - ITU-T Recommendation Q.713, section 3.17
-record(segmentation,
		{first :: undefined | boolean(),
		class :: undefined | 0..1,
		remaining_seg :: undefined | 0..15,
		seg_local_ref ::  0..16777215}).

%% SCCP messages - ITU-T Recommendation Q.713, section 4
-record(sccp_connection_req,
		{src_local_ref :: undefined | 0..16777215,
		class :: undefined | 2..3,
		called_party :: undefined | #party_address{},
		credit :: undefined | byte(),
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		hop_counter :: undefined | 1..15,
		importance :: undefined | 0..7}).

-record(sccp_connection_confirm,
		{dest_local_ref :: undefined | 0..16777215,
		src_local_ref :: undefined | 0..16777215,
		class :: undefined | 2..3,
		credit :: undefined | byte(),
		called_party :: undefined | #party_address{},
		data :: undefined | binary(),
		importance :: undefined | 0..7}).

-record(sccp_connection_refused,
		{dest_local_ref :: undefined | 0..16777215,
		refusal_cause :: undefined | enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_oirg | dest_unknown | dest_inaccessible | network_resource
				| access_failure | access_congestion | subsystem_failure
				| subsystem_congestion | connection_expire | incomp_userdata
				| reserved | unqualified | hcounter_violation | sccp_failure
				| no_translation_addr | unequipped_error,
		called_party :: undefined | #party_address{},
		data :: undefined | binary(),
		importance :: undefined | 0..7}).

-record(sccp_released,
		{dest_local_ref :: undefined | 0..16777215,
		src_local_ref :: undefined | 0..16777215,
		release_cause :: undefined | enduser_orig | enduser_congestion | enduser_failure
				| sccp_user_originated | rpc_error | inconsistant_data | access_failure
				| access_congestion | subsystem_failure | subsystem_congestion
				| mtp_failure | network_congestion | reset_timer_expire
				| inactivity_timer_expire | unqualified | sccp_failure,
		data :: undefined | binary(),
		importance :: undefined | 0..7}).

-record(sccp_release_complete,
		{dest_local_ref :: undefined | 0..16777215,
		src_local_ref :: undefined | 0..16777215}).

-record(sccp_data_form1,
		{dest_local_ref :: undefined | 0..16777215,
		segmenting :: undefined | boolean(),
		data :: undefined | binary()}).

-record(sccp_data_form2,
		{dest_local_ref :: undefined | 0..16777215,
		sequencing :: undefined | #sequencing{},
		data :: undefined | binary()}).

-record(sccp_data_ack,
		{dest_local_ref :: undefined | 0..16777215,
		sequencing :: undefined | #sequencing{},
		credit :: undefined | byte()}).

-record(sccp_unitdata,
		{class :: undefined | 0..1 | 128..129,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary()}).

-record(sccp_unitdata_service,
		{return_cause :: undefined | no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary()}).

-record(sccp_expedited_data,
		{dest_local_ref :: undefined | 0..16777215,
		data :: undefined | binary()}).

-record(sccp_expedited_ack,
		 {dest_local_ref :: undefined | 0..16777215}).

-record(sccp_reset_request,
		{dest_local_ref :: undefined | 0..16777215,
		src_local_ref :: undefined | 0..16777215,
		reset_cause :: undefined | enduser_orig, sccp_user_oirg, incorrect_ps,
				incorrect_pr, rpc_error, remote_end_user_operational,
				network_operational, access_opertional, network_congestion,
				reserved, unqualified}).

-record(sccp_reset_confirmation,
		{dest_local_ref :: undefined | 0..16777215,
		src_local_ref :: undefined | 0..16777215}).

-record(sccp_protocol_data_unit_error,
		{dest_local_ref :: undefined | 0..16777215,
		error_cause :: undefined | integer()}).
		
-record(sccp_inactivity_test,
		{dest_local_ref :: undefined | 0..16777215,
		src_local_ref :: undefined | 0..16777215,
		class :: undefined | 2..3,
		sequencing :: undefined | #sequencing{},
		credit :: undefined | byte()}).

-record(sccp_extended_unitdata,
		{class :: undefined | 0..1 | 128..129,
		hop_counter :: undefined | 1..15,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | 0..7}).

-record(sccp_extended_unitdata_service,
		{return_cause :: undefined | no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		hop_counter :: undefined | 1..15,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | 0..7}).

-record(sccp_long_unitdata,
		{class :: undefined | 0..1 | 128..129,
		hop_counter :: undefined | 1..15,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | 0..7}).

-record(sccp_long_unitdata_service,
		{return_cause :: undefined | no_translation | subsystem_congestion | subsystem_failure
				| unequipped_user | mtp_failure | network_congestion | unqualified
				| transport_error | processing_error | reassembly_fail | sccp_failure
				| hcounter_violation | seg_not_supported | seg_failure | reserved,
		hop_counter :: undefined | 1..15,
		called_party :: undefined | #party_address{},
		calling_party :: undefined | #party_address{},
		data :: undefined | binary(),
		segmentation :: undefined | #segmentation{},
		importance :: undefined | 0..7}).


%%% sccp_primitive.hrl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @copyright 2025 SigScale Global Inc.
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
%%%  ITU-T recommendation Q.711 Functional Decsription of
%%%  the Signalling Connection Control Part describes the
%%%  primitives and their parameters used in the N-Service
%%%  interface.  Each primitive has a record defined here
%%%  containing it's parameters.  Modules using this service
%%%  utilize these records to format messages:
%%%
%%%  {'N', 'N-UNITDATA', request, Parameters}
%%%  		when is_record(Parameters, 'N-UNITDATA')
%%%

%% reference: Table 2/Q.711 - Parameters of the primitive N-CONNECT
-record('N-CONNECT',
		{calledAddress ::  sccp_codec:party_address() | undefined,
		callingAddress :: sccp_codec:party_address() | undefined,
		respondAddress :: sccp_codec:party_address() | undefined,
		expeditedData :: binary() | undefined,
		qos,
		userData :: binary() | undefined,
		connectionID,
		importance :: 0..7 | undefined}).

%% reference: Table 3/Q.711 - Parameters of the primitive N-DATA
-record('N-DATA',
		{userData :: binary(),
		connectionID,
		importance :: 0..7 | undefined}).

%% reference: Table 4/Q.711 - Parameters of the primitive N-EXPEDITED-DATA
-record('N-EXPEDITED-DATA',
		{userData :: binary(),
		connectionID}).

%% reference: Table 5/Q.711 - Parameters of the primitive N-RESET
-record('N-RESET',
		{originator :: network_service_provider
				| network_service_user
				| undefined,
		reason :: network_service_provider_congestion
				| local_sccp_originated
				| user_synchronization
				| reason_unspecified
				| undefined,
		connectionID}).

%% reference: Table 6/Q.711 - Parameters of the primitive N-DISCONNECT
-record('N-DISCONNECT',
		{originator :: network_service_provider
				| network_service_user
				| undefined,
		respondAddress :: sccp_codec:party_address() | undefined,
		reason,
		userData :: binary() | undefined,
		connectionID,
		importance :: 0..7 | undefined}).

%% reference: Table 8/Q.711 - Parameters of the primitive N-INFORM
-record('N-INFORM',
		{reason :: network_service_provider_failure
				| network_service_congestion
				| network_service_provider_qos_change
				| network_service_user_failure
				| network_service_user_congestion
				| network_service_user_qos_change
				| reason_unspecified,
		connectionID,
		qos}).

%% reference: Table 12/Q.711 - Parameters of the primitive N-UNITDATA
-record('N-UNITDATA',
		{calledAddress :: sccp_codec:party_address(),
		callingAddress :: sccp_codec:party_address() | undefined,
		sequenceControl :: boolean() | undefined,
		returnOption :: boolean() | undefined,
		importance :: 0..7 | undefined,
		userData :: binary()}).

%% reference: Table 13/Q.711 - Parameters of the primitive N-NOTICE
-record('N-NOTICE',
		{calledAddress :: sccp_codec:party_address(),
		callingAddress :: sccp_codec:party_address(),
		reason :: no_translation_for_address_nature
				| no_translation_for_address
				| subsystem_congestion
				| subsystem_failure
				| unequipped_user
				| mtp_failure
				| network_congestion
				| sccp_unqualified
				| error_in_message_transport
				| error_in_local_processing
				| destination_cannot_perfom_reassembly
				| sccp_failure
				| hop_counter_violation
				| segmentation_not_supported
				| segmentation_failed,
		userData :: binary(),
		importance :: 0..7 | undefined}).

%% reference: Table 15/Q.711 - Parameters of the primitive N-COORD
-record('N-COORD',
		{affectedSubsystem :: sccp_codec:party_address(),
		multiplicity :: pos_integer() | undefined}).

%% reference: Table 16/Q.711 - Parameters of the primitive N-STATE
-record('N-STATE',
		{affectedSubsystem :: sccp_codec:party_address(),
		userStatus :: user_in_service | user_out_of_service,
		multiplicity :: pos_integer() | undefined}).

%% reference: Table 17/Q.711 - Parameters of the primitive N-PCSTATE
-record('N-PCSTATE',
		{affectedSignallingPoint :: 1..16383,
		signallingPointStatus :: accessible | inaccessible | congested,
		remoteSCCPStatus :: available
				| unavailable
				| unequipped
				| inaccessible
				| congested
				| undefined,
		restrictedImportanceLevel :: 0..7 | undefined}).


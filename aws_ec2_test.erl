%%%
%%% Copyright (c) 2007 Bernhard H. Damberger 
%%% All rights reserved.
%%% 
%%% Developed by: 		Bernhard H. Damberger
%%%                     bernied at gmail dot com
%%%                     http://code.google.com/p/erlawys/
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the
%%% "Software"), to deal with the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to
%%% permit persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%% 
%%% Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimers.
%%% Redistributions in binary form must reproduce the above
%%% copyright notice, this list of conditions and the following disclaimers
%%% in the documentation and/or other materials provided with the
%%% distribution.
%%% Neither the names of Bernhard H. Damberger,
%%% nor the names of its contributors may be used to endorse
%%% or promote products derived from this Software without specific prior
%%% written permission.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
%%% ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
%%%

%% A simple test for the AWS interface.

-module(aws_ec2_test).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/07/31').

-compile(export_all).
-export([test_simple/2]).

-define(LAUNCH_INSTANCE, "ami-23b6534a").

%% Example session:
%% > erl
%% Erlang (BEAM) emulator version 5.5.5 [source] [async-threads:0] [kernel-poll:false]
%%
%% Eshell V5.5.5  (abort with ^G)
%% 1> Key = "...".
%% "..."
%% 2> AccessKey = "...".
%% "..."
%% 3> c(aws_ec2_xml).
%% {ok,aws_ec2_xml}
%% 4> c(aws_ec2).
%% {ok,aws_ec2}
%% 5> c(aws_ec2_test).
%% {ok,aws_ec2_test}
%% 6> aws_ec2_test:init_tests().
%% ok
%% 7> aws_ec2_test:test_simple(Key, AccessKey).
%% Attempting to Launch first instance: {"ami-23b6534a",
%% ...

%% To run this test/example you must first execute init_tests()
%% so that erlang has a chance to initialize internal objects etc.
init_tests() ->
	crypto:start(),
	inets:start(),
	ssl:start().


%% This is a test/example program which will
%% 1. Launch an instance from image called ami-23b6534a.
%% 2. Launch another instance from the same image.
%% 3. Wait for 5 seconds for instances to settle.
%% 4. Query current status of all instances.
%% 5. Wait for first launched instance to go to "running" state.
%% 6. Wait for 60 seconds to allow instance to launch httpd.
%% 7. Requests html from first running instance.
%% 8. Gets list of running instances to terminate.
%% 9. Terminates instances.
%%10. Shows list of terminated instances.
%%11. Shows final status of current AWS session for user.
%%
%% Note, we need the try_again() function to force the API to be
%% called again if it failed previously. For some reason, the AWS
%% calls don't always make it through. So we use this hack to 
%% recall the API. Need to figure out if its my code, erlang, or AWS
%% that is causing the problem. Perhaps its just the nature of 
%% the thing.
test_simple(Key, AccessKey) ->
	test_simple(Key, AccessKey, 10).
test_simple(Key, AccessKey, Count) ->
	% Get Description of all images.
	Instances = try_again(
		fun(K, A) -> aws_ec2:describe_images(K, A) end,
		[Key, AccessKey],
		Count),
		
	% Pull out information on AMI we are interested in. See LAUNCH_INSTANCE var.
	MyInstance = extract_instance(?LAUNCH_INSTANCE, Instances),
	
	% Now run the image.
	io:format("Attempting to Launch first instance: ~p~n", [MyInstance]),
	LaunchedInstance = try_again(
		fun(K, A, E) -> aws_ec2:run_instance(K, A, E) end,
		[Key, AccessKey, element(1, MyInstance)],
		Count),
	io:format("Launched first instance ~p~n", [LaunchedInstance]),
	
	% Run the same image a second time.
	io:format("Attempting to Launch second instance ~p~n", [MyInstance]),
	LaunchedInstance2 = try_again(
		fun(K, A, E) -> aws_ec2:run_instance(K, A, E) end,
		[Key, AccessKey, element(1, MyInstance)],
		Count),
	io:format("Launched second instance ~p~n", [LaunchedInstance2]),
	io:format("Wait for 5 seconds for instances to settle in.~n"),
	sleep(5000),
	
	% Get a description of whats going on.
	DescribedInstances = try_again(
		fun(K, A) -> aws_ec2:describe_instances(K, A) end,
		[Key, AccessKey],
		Count),
	io:format("Current status of instances is ~n~p~n", [DescribedInstances]),
	
	% Get information on first instance so we can manipulate it.
	FirstInstanceId = extract_instance_id(LaunchedInstance),
	
	% Wait until the first instance is running.
	io:format("Wait until instance ~p is running...~n", [FirstInstanceId]),
	DnsName = poll_until_not_pending(Key, AccessKey, FirstInstanceId, Count),
	io:format("Instance ~p is now running! Wait for 1 minute so it can start up httpd.~n", [FirstInstanceId]),
	
	% After instance starts up wait for one minute to allow HTTPD to start up.
	sleep(60000),
	
	% Run a request for the root document at the first instance.
	io:format("Requesting HTTP from instance @ ~p~n", [DnsName]),
	Result = try_again(
		fun(U) -> http:request(U) end,
		["http://" ++ DnsName],
		Count),
	io:format("HTTP result is~n~p~n", [Result]),
	
	% Get list of all instances (including already terminated, should probably filter them).
	InstancesToTerminate = extract_instances_to_terminate(DescribedInstances),
	io:format("Will terminate instances~n~p~n", [InstancesToTerminate]),
	
	% Terminate the instances.
	DeadInstances = try_again(
		fun(K, A, L) -> aws_ec2:terminate_instances(K, A, L) end,
		[Key, AccessKey, InstancesToTerminate],
		Count),
	io:format("Terminated instances~n~p~n", [DeadInstances]),
	
	% Tell user what the current state is.
	FinalState = try_again(
		fun(K, A) -> aws_ec2:describe_instances(K, A) end,
		[Key, AccessKey],
		Count),
	io:format("Final status of instances is ~n~p~n", [FinalState]).
	
	% We are DONE!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Methods used to run the test case.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Polls AWS to see if the image is running or not.
poll_until_not_pending(Key, AccessKey, Instance, Count) ->
	DescribedInstances = try_again(
		fun(K, A, I) -> aws_ec2:describe_instances(K, A, I) end,
		[Key, AccessKey, [Instance]],
		Count),
	[{_, _, _, [{Instance, _, _, State, _, DnsAddress, _, _}]}] = DescribedInstances,
	if State =:= "running" andalso DnsAddress =/= null ->
		DnsAddress;
	true ->
		sleep(10000),
		poll_until_not_pending(Key, AccessKey, Instance, Count)
	end.

%% Sleep for T millis.
sleep(T) ->
	receive
	after
		T ->
			true
	end.

%% Try to execute function F Count times before giving up.
%% Negative value will never stop.
try_again(F, Params, Count) ->
	try apply(F, Params)
	catch
		error:Term ->
			if Count =/= 0 ->
				io:format("Failed in calling ~p on count ~p~n", [F, Count]),
				try_again(F, Params, Count-1);
			true ->
				{error, Term}
			end
	end.
	
%% Extract Instance w/ name Name.
extract_instance(Name, [{Name, _, _, _, _}=R|_T]) -> R;
extract_instance(Name, [_H|T]) -> extract_instance(Name, T);
extract_instance(_Name, []) -> null.

%% Extract instance id from description.
extract_instance_id({_, _, _, [{Instance, _, _, _, _, _, _, _}]}) ->
	Instance;
extract_instance_id(_X) ->
	null.

%% Extract DNS name. Not used, since we get that from poll_until_not_pending().
extract_dns_name([{_, _, _, [{_, _, _, _, _, DnsName, _, _}]}|_T]) ->
	DnsName;
extract_dns_name(_X) ->
	null.

%% Get list of all instances we need to terminate.
%% Note, should probably filter already terminated instances.
extract_instances_to_terminate(L) ->
	extract_instances_to_terminate(L, []).
extract_instances_to_terminate([{_, _, _, Instances}|T], L) ->
	extract_instances_to_terminate(T, extract_instances_from_list(Instances, L));
extract_instances_to_terminate([], L) ->
	L.
	
%extract_instances_from_list([{InstanceId, _, _, "running", _, _, _, _}|T], L) ->
extract_instances_from_list([{InstanceId, _, _, _, _, _, _, _}|T], L) ->
	[InstanceId|extract_instances_from_list(T, L)];
extract_instances_from_list([_H|T], L) ->
	extract_instances_from_list(T, L);
extract_instances_from_list([], L) ->
	L.

	
	
	
	
	
	
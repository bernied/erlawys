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

-module(aws_ec2).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/07/28').

-include_lib("xmerl/include/xmerl.hrl").

-compile(export_all).
-export([authorize_security_group_ingress/5,	
		authorize_security_group_ingress/7,	
		confirm_product_instance/4,	
		create_key_pair/3,	
		create_security_group/4,	
		delete_key_pair/3,	
		delete_security_group/3,	
		deregister_image/3,	
		describe_image_attribute/4,	
		describe_images/2,
		describe_images/5,	
		describe_instance/3,	
		describe_instances/2,	
		describe_instances/3,	
		describe_key_pairs/3,	
		describe_security_groups/3,	
		get_console_output/3,	
		modify_image_attribute/8,	
		reboot_instance/3,	
		reboot_instances/3,	
		register_image/3,	
		reset_image_attribute/4,	
		revoke_security_group_ingress/5,
		revoke_security_group_ingress/7,
		run_instance/3,	
		run_instances/9,
		terminate_instance/3,	
		terminate_instances/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Methods used to construct URLs to access AWS.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Is the response an error?
is_error(Result) ->
	Error = xmerl_xpath:string("//Errors[1]/Error/Code", Result),
	if Error =:= [] ->
		false;
	true ->
		true
	end.
	
% Checks to see if an error occured.
scan_xml(Xml) ->
	{ Result, _Rest } = xmerl_scan:string(Xml),
	IsError = is_error(Result),
	if IsError =:= true ->
		capture_errors(Result);
	true ->
		Result
	end.
	
% An error has occured, so extract them out and present them.
capture_errors(Error) ->
	{error, process_sequence(Error, fun(X,Y) -> extract_error(X,Y) end)}.

extract_error(Seq, Count) ->
	PrefixPath = sequence_exists(Seq, Count, "//Errors/Error"),
	if PrefixPath =:= [] ->
		null;
	true ->
		[ #xmlText{value=Code} ] = xpath(PrefixPath ++ "/Code/text()", Seq),
		[ #xmlText{value=Message} ]= xpath(PrefixPath ++ "/Message/text()", Seq),
		{Code, Message}
	end.
	
% Filters out errors for the GetValue function.
return_value({error, Errors}, _GetValue, _Params) ->
	{error, Errors};
return_value(Result, GetValue, Params) ->
	GetValue(Result, Params).

% Returns a string
return_string(Xml, Name) ->
%	Result = scan_xml(Xml),
	return_value(Xml, fun(R,N) -> extract_string(R, N) end, Name).
	
% Uses XPath to extract a string from Result.
extract_string(Result, Name) ->
	Xpath = "//" ++ Name ++ "/text()",
	[ #xmlText{value=Str} ]  = xpath(Xpath, Result),
	Str.

% Returns a boolean.
return_boolean(Xml) -> return_boolean(Xml, "return").
return_boolean(Xml, Name) ->
%	Result = scan_xml(Xml),
	return_value(Xml, fun(R,N) -> extract_boolean(R, N) end, Name).
	
% Uses XPath to extract a boolean from Result
extract_boolean(Result, Name) ->
	Value = extract_string(Result, Name),
	if Value =:= "true" ->
		true;
	true ->
		false
	end.

% Processes a sequence of elements in Xml document. Note this is kinda hackey.
% If I could get xpath:count(Nodes) to work, then this might be much simpler.
% Also depends on there being a contigious sequence of elements before we stop,
% which happens not to be the case in some cases.
process_sequence(Seq, Extractor) ->
	process_sequence(Seq, Extractor, 1).
process_sequence(Seq, Extractor, Count) ->
	process_sequence(Seq, Extractor, Count, []).
process_sequence(Seq, Extractor, Count, Prefix) ->
	process_sequence(Seq, Extractor, Count, Prefix, []).
process_sequence({error, Errors}, _Extractor, _Count, _Prefix, _Result) ->
	{error, Errors};
process_sequence(Seq, Extractor, Count, Prefix, Result) ->
	Data = Extractor(Seq, Count, Prefix),
	if null =:= Data ->
		Result;
	true ->
		[Data|process_sequence(Seq, Extractor, Count+1, Prefix, Result)]
	end.

sequence_exists(Data, Count, PrefixPath) ->
	Xpath = PrefixPath ++ "[" ++ integer_to_list(Count) ++ "]",
	Parent = xmerl_xpath:string(Xpath, Data),
	if Parent =:= [] ->
		[];
	true ->
		Xpath
	end.

% Uses XPath to find and extract element from Seq.
% If nothing there or an exception is tossed returns an empty structure
% from which we can extract null. Thus the code doing the pattern matching
% does not need to handle this special case.
xpath(Xpath, Seq) ->
	try
		Result = xmerl_xpath:string(Xpath, Seq),
		if Result =:= [] ->
			[ #xmlText{value=null} ];
		true ->
			Result
		end
	catch
		throw:_ ->
			[ #xmlText{value=null} ];
		error:_ ->
			[ #xmlText{value=null} ]
	end.

% Process describe images.
process_describe_images(Xml) ->
    Items = scan_xml(Xml),
	process_sequence(Items, fun(S, C, P) -> extract_item(S, C, P) end, 2). % LAMb: work around for some issue where first element is []!
    
extract_item(Seq, Count, _Prefix) ->
	Xpath = sequence_exists(Seq, Count, "//item"),
	if Xpath =:= [] ->
		null;
	true ->
		[ #xmlText{value=ImageId} ]  = xpath(Xpath ++ "/imageId/text()", Seq),
		[ #xmlText{value=ImageLocation} ] = xpath(Xpath ++ "/imageLocation/text()", Seq),
		[ #xmlText{value=ImageState} ] = xpath(Xpath ++ "/imageState/text()", Seq),
		[ #xmlText{value=ImageOwnerId} ] = xpath(Xpath ++ "/imageOwnerId/text()", Seq),
		[ #xmlText{value=IsPublic} ] = xpath(Xpath ++ "/isPublic/text()", Seq),
		{ ImageId, ImageLocation, ImageState, ImageOwnerId, IsPublic }
	end.

% Process runinstance result.
process_run_instance(Xml) ->
	Instances = scan_xml(Xml),
	extract_run_instance(Instances).

extract_run_instance(Instances) ->
	Xpath = sequence_exists(Instances, 1, "//item"),
	if Xpath =:= [] ->
		null;
	true ->
		GroupIds = process_sequence(Instances, fun(S, C, P) -> extract_group_ids(S, C, P) end, 1, "/"),
		RunInstances = process_sequence(Instances, fun(S, C, P) -> extract_instance_set(S, C, P) end, 1, "/"),
		[ #xmlText{value=ReservationId} ]  = xpath("//reservationId/text()", Instances),
		[ #xmlText{value=OwnerId} ] = xpath("//ownerId/text()", Instances),
		{ReservationId, OwnerId, GroupIds, RunInstances}
	end.

% Process describe instances.
process_describe_instances(Xml) ->
	Instances = scan_xml(Xml),
	process_sequence(Instances, fun(S, C, P) -> extract_instance(S, C, P) end, 1, "//reservationSet").

	
extract_instance(Instances, Count, Prefix) ->
	Xpath = sequence_exists(Instances, Count, Prefix ++ "/item"),
%	io:format("~p~n", [Xpath]),
	if Xpath =:= [] ->
		null;
	true ->
		GroupIds = process_sequence(Instances, fun(S, C, P) -> extract_group_ids(S, C, P) end, 1, Xpath),
		RunInstances = process_sequence(Instances, fun(S, C, P) -> extract_instance_set(S, C, P) end, 1, Xpath),
		[ #xmlText{value=ReservationId} ]  = xpath(Xpath ++ "/reservationId/text()", Instances),
		[ #xmlText{value=OwnerId} ] = xpath(Xpath ++ "/ownerId/text()", Instances),
		{ReservationId, OwnerId, GroupIds, RunInstances}
	end.

extract_group_ids(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/groupSet/item"),
	if Xpath =:= [] ->
		null;
	true ->
		[ #xmlText{value=GroupId} ]  = xpath(Xpath ++ "/groupId/text()", Seq),
		GroupId
	end.

extract_instance_set(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/instancesSet/item"),
	if Xpath =:= [] ->
		null;
	true ->
		[ #xmlText{value=InstanceId} ]  = xpath(Xpath ++ "/instanceId/text()", Seq),
		[ #xmlText{value=ImageId} ]  = xpath(Xpath ++ "/imageId/text()", Seq),
		[ #xmlText{value=InstanceCode} ]  = xpath(Xpath ++ "/instanceState/code/text()", Seq),
		[ #xmlText{value=InstanceName} ]  = xpath(Xpath ++ "/instanceState/name/text()", Seq),
		[ #xmlText{value=PrivateDnsName} ]  = xpath(Xpath ++ "/privateDnsName/text()", Seq),
		[ #xmlText{value=DnsName} ]  = xpath(Xpath ++ "/dnsName/text()", Seq),
		[ #xmlText{value=Reason} ]  = xpath(Xpath ++ "/reason/text()", Seq),
		[ #xmlText{value=AmiLaunchIndex} ]  = xpath(Xpath ++ "/amiLaunchIndex/text()", Seq),
		{InstanceId, ImageId, InstanceCode, InstanceName, PrivateDnsName, DnsName, Reason, AmiLaunchIndex}
	end.

% Process describe image attribute.
process_describe_image_attribute(Xml) ->
%	Attr = scan_xml(Xml),
	Xml.	% LAMb: fix this. Requires determining what the return type is.

% Process Key/Pair descriptions.
process_describe_key_pairs(Xml) ->
	Pairs = scan_xml(Xml),
	process_sequence(Pairs, fun(S, C, P) -> extract_key_pairs(S, C, P) end, 1, "//keySet").

extract_key_pairs(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/item"),
	if Xpath =:= [] ->
		null;
	true ->
		[ #xmlText{value=KeyName} ]  = xpath(Xpath ++ "/keyName/text()", Seq),
		[ #xmlText{value=KeyFingerprint} ]  = xpath(Xpath ++ "/keyFingerprint/text()", Seq),
		{KeyName, KeyFingerprint}
	end.
	
% Process describe security group response.
process_describe_security_group(Xml) ->
	Group = scan_xml(Xml),
	process_sequence(Group, fun(S, C, P) -> extract_security_group(S, C, P) end, 1, "//securityGroupInfo").
	
extract_security_group(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/item"),
	if Xpath =:= [] ->
		null;
	true ->
		IpPermissions = process_sequence(Seq, fun(S, C, P) -> extract_ip_permissions(S, C, P) end, 1, Xpath),
		[ #xmlText{value=OwnerId} ]  = xpath(Xpath ++ "/ownerId/text()", Seq),
		[ #xmlText{value=GroupName} ]  = xpath(Xpath ++ "/groupName/text()", Seq),
		[ #xmlText{value=GroupDescription} ]  = xpath(Xpath ++ "/groupDescription/text()", Seq),
		{OwnerId, GroupName, GroupDescription, IpPermissions}
	end.

extract_ip_permissions(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/ipPermissions/item"),
	if Xpath =:= [] ->
		null;
	true ->
		IpRanges = process_sequence(Seq, fun(S, C, P) -> extract_ip_ranges(S, C, P) end, 1, Xpath),
		[ #xmlText{value=IpProtocol} ]  = xpath(Xpath ++ "/ipProtocol/text()", Seq),
		[ #xmlText{value=FromPort} ]  = xpath(Xpath ++ "/fromPort/text()", Seq),
		[ #xmlText{value=ToPort} ]  = xpath(Xpath ++ "/toPort/text()", Seq),
%		[ #xmlText{value=PreviousCode} ]  = xpath(Xpath ++ "/groups/text()", Seq),
		{IpProtocol, FromPort, ToPort, IpRanges}
	end.
	
extract_ip_ranges(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/ipRanges/item"),
	if Xpath =:= [] ->
		null;
	true ->
		[ #xmlText{value=CidrIp} ]  = xpath(Xpath ++ "/cidrIp/text()", Seq),
		CidrIp
	end.
	
% Process terminate instances.
process_terminate_instances(Xml) ->
	Instances = scan_xml(Xml),
	process_sequence(Instances, fun(S, C, P) -> extract_dead_instance(S, C, P) end, 1, "//instancesSet").
	
extract_dead_instance(Seq, Count, Prefix) ->
	Xpath = sequence_exists(Seq, Count, Prefix ++ "/item"),
	if Xpath =:= [] ->
		null;
	true ->
		[ #xmlText{value=InstanceId} ]  = xpath(Xpath ++ "/instanceId/text()", Seq),
		[ #xmlText{value=ShutdownCode} ]  = xpath(Xpath ++ "/shutdownState/code/text()", Seq),
		[ #xmlText{value=ShutdownName} ]  = xpath(Xpath ++ "/shutdownState/name/text()", Seq),
		[ #xmlText{value=PreviousCode} ]  = xpath(Xpath ++ "/previousState/code/text()", Seq),
		[ #xmlText{value=PreviousName} ]  = xpath(Xpath ++ "/previousState/name/text()", Seq),
		{InstanceId, ShutdownCode, ShutdownName, PreviousCode, PreviousName}
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual AWS API calls.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
%%
%% AuthorizeSecurityGroupIngress
%%
authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId
	) ->
	Xml = aws_ec2_xml:authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp
	) ->
	Xml = aws_ec2_xml:authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp),
	Result = scan_xml(Xml),
	return_boolean(Result).

%%
%% ConfirmProductInstance
%%

confirm_product_instance(Key, AccessKey,
		ProductCode,
		InstanceId
	) ->
	Xml = aws_ec2_xml:confirm_product_instance(Key, AccessKey, ProductCode, InstanceId),
	Result = scan_xml(Xml),
	IsError = is_error(Result),
	if IsError =:= true ->
		Result;
	true ->
		Value = return_boolean(Result, "result"),
		OwnerId = return_string(Result, "ownerId"),
		{Value, OwnerId}
	end.
	
%%
%% CreateKeyPair
%%

create_key_pair(Key, AccessKey, KeyName) ->
	Xml = aws_ec2_xml:create_key_pair(Key, AccessKey, KeyName),
	Result = scan_xml(Xml),
	IsError = is_error(Result),
	if IsError =:= true ->
		Result;
	true ->
		KeyName = return_string(Result, "keyName"),
		KeyFingerprint = return_string(Result, "keyFingerprint"),
		KeyMaterial = return_string(Result, "keyMaterial"),
		{KeyName, KeyFingerprint, KeyMaterial}
	end.
	
%%
%% CreateSecurityGroup
%%

create_security_group(Key, AccessKey, GroupName, GroupDescription) ->
	Xml = aws_ec2_xml:create_security_group(Key, AccessKey, GroupName, GroupDescription),
	Result = scan_xml(Xml),
	return_boolean(Result).

	
%%
%% DeleteKeyPair
%%

delete_key_pair(Key, AccessKey, KeyName) ->
	Xml = aws_ec2_xml:delete_key_pair(Key, AccessKey, KeyName),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
%%
%% DeleteSecurityGroup
%%

delete_security_group(Key, AccessKey, GroupName) ->
	Xml = aws_ec2_xml:delete_security_group(Key, AccessKey, GroupName),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
%%
%% DeregisterImage
%%

deregister_image(Key, AccessKey, ImageId) ->
	Xml = aws_ec2_xml:deregister_image(Key, AccessKey, ImageId),
	Result = scan_xml(Xml),
	return_boolean(Result).

	
%%
%% DescribeImageAttribute
%%

describe_image_attribute(Key, AccessKey,
		ImageId,
		Attribute
	) ->
	Xml = aws_ec2_xml:describe_image_attribute(Key, AccessKey, ImageId, Attribute),
	process_describe_image_attribute(Xml).
	
%%
%% DescribeImages
%%

describe_images(Key, AccessKey) -> describe_images(Key, AccessKey, [], [], []).
describe_images(Key, AccessKey, ImageId_n, Owner_n, ExecutableBy_n) ->
	Xml = aws_ec2_xml:describe_images(Key, AccessKey,
		ImageId_n,
		Owner_n,
		ExecutableBy_n),
	process_describe_images(Xml).
	
%%
%% DescribeInstances
%%

describe_instance(Key, AccessKey, InstanceId) -> describe_instances(Key, AccessKey, [InstanceId]).
describe_instances(Key, AccessKey) ->
	describe_instances(Key, AccessKey, []).
describe_instances(Key, AccessKey, InstanceId_n) ->
	Xml = aws_ec2_xml:describe_instances(Key, AccessKey, InstanceId_n),
	process_describe_instances(Xml).
	
%%
%% DescribeKeyPairs
%%

describe_key_pairs(Key, AccessKey, KeyName_n) ->
	Xml = aws_ec2_xml:describe_key_pairs(Key, AccessKey, KeyName_n),
	process_describe_key_pairs(Xml).
	
%%
%% DescribeSecurityGroups
%%

describe_security_groups(Key, AccessKey, GroupName_n) ->
	Xml = aws_ec2_xml:describe_security_groups(Key, AccessKey, GroupName_n),
	process_describe_security_group(Xml).
	
%%
%% GetConsoleOutput
%%

get_console_output(Key, AccessKey, InstanceId) ->
	Xml = aws_ec2_xml:get_console_output(Key, AccessKey, InstanceId),
	Result = scan_xml(Xml),
	IsError = is_error(Result),
	if IsError =:= true ->
		Result;
	true ->
	%	ResultInstanceId = return_string(Result, "instanceId"), % LAMb: does not work for some reason!
		Timestamp = return_string(Result, "timestamp"),
		Output = return_string(Result, "output"),
	%	{ResultInstanceId, Timestamp, Output}.
		{InstanceId, Timestamp, Output}
	end.
	
%%
%% ModifyImageAttribute
%%

modify_image_attribute(Key, AccessKey,
		ImageId,
		Attribute,
		OperationType,
		UserId_n,
		UserGroup_n,
		ProductCode_n
	) ->
	Xml = aws_ec2_xml:modify_image_attribute(Key, AccessKey,
		ImageId,
		Attribute,
		OperationType,
		UserId_n,
		UserGroup_n,
		ProductCode_n),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
%%
%% RebootInstances
%%

reboot_instance(Key, AccessKey, InstanceId) -> reboot_instances(Key, AccessKey, [InstanceId]).
reboot_instances(Key, AccessKey,InstanceId_n) ->
	Xml = aws_ec2_xml:reboot_instances(Key, AccessKey, InstanceId_n),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
%%
%% RegisterImage
%%

register_image(Key, AccessKey, ImageLocation) ->
	Xml = aws_ec2_xml:register_image(Key, AccessKey, ImageLocation),
	return_string(Xml, "imageId").
	
%%
%% ResetImageAttribute
%%

reset_image_attribute(Key, AccessKey,
		ImageId,
		Attribute
	) ->
	Xml = aws_ec2_xml:reset_image_attribute(Key, AccessKey, ImageId, Attribute),
	Result = scan_xml(Xml),
	return_boolean(Result).

	
%%
%% RevokeSecurityGroupIngress
%%

revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId
	) ->
	Xml = aws_ec2_xml:revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp
	) ->
	Xml = aws_ec2_xml:revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp),
	Result = scan_xml(Xml),
	return_boolean(Result).
	
%%
%% RunInstances
%%

run_instance(Key, AccessKey, ImageId) -> run_instances(Key, AccessKey, ImageId, 1, 1, null, [], null, null).
run_instances(Key, AccessKey,
		ImageId,
		MinCount,
		MaxCount,
		KeyName,
		SecurityGroup_n,
		UserData,
		AddressingType
	) ->
	Xml = aws_ec2_xml:run_instances(Key, AccessKey,
		ImageId,
		MinCount,
		MaxCount,
		KeyName,
		SecurityGroup_n,
		UserData,
		AddressingType),
	Xml,
	process_run_instance(Xml).
	
%%
%% TerminateInstances
%%

terminate_instance(Key, AccessKey, InstanceId) -> terminate_instances(Key, AccessKey, [InstanceId]).
terminate_instances(Key, AccessKey, InstanceId_n) ->
	Xml = aws_ec2_xml:terminate_instances(Key, AccessKey, InstanceId_n),
	process_terminate_instances(Xml).
	
	
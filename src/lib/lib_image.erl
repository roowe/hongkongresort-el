-module(lib_image).
-export([image/1]).

-export([activity_images/1]).

-include("db_image.hrl").
-include("db_activity_image_relation.hrl").
-include("define_time.hrl").
-include("common.hrl").

image(Id) ->
    ets_cache:get_with_default(image_cache, Id,
                               fun() ->
                                       case db_image:image(Id) of
                                           {ok, [Value]} ->
                                               {expiration, Value, ?FOUR_HOUR_SECONDS};
                                           {ok, []} ->
                                               {expiration, [], ?FOUR_HOUR_SECONDS};
                                           {error, Error} ->
                                               ?WARNING_MSG("db error ~p~n", [Error]),
                                               []
                                       end
                               end).


activity_images(ActivityId) ->
    case db_activity_image_relation:activity_image_relation(ActivityId) of
        {error, Error} ->
            ?WARNING_MSG("db error ~p~n", [Error]),
            [];
        {ok, List} ->
            lists:foldl(fun(#activity_image_relation{
                               image_id = ImageId
                              }, Acc) ->
                                case image(ImageId) of
                                    [] ->
                                        Acc;
                                    Image ->
                                        [Image|Acc]
                                end
                        end, [], List)
    end.

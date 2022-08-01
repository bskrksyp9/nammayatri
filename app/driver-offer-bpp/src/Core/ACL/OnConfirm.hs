module Core.ACL.OnConfirm (mkOnConfirmMessage) where

import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.OnConfirm as OnConfirm
import Beckn.Types.Id
import qualified Core.ACL.Common as Common
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Booking.BookingLocation as DBL
import Product.FareCalculator.Calculator
import Utils.Common (amountToDecimalValue)

mkOnConfirmMessage :: UTCTime -> DConfirm.DConfirmRes -> OnConfirm.OnConfirmMessage
mkOnConfirmMessage now res = do
  let booking = res.booking
  let vehicleVariant = Common.castVariant res.booking.vehicleVariant
  let itemCode = OnConfirm.ItemCode OnConfirm.ONE_WAY_TRIP vehicleVariant Nothing Nothing
      fareParams = booking.fareParams
      totalFare = fareSum fareParams
      totalFareDecimal = amountToDecimalValue totalFare
      currency = "INR"
  OnConfirm.OnConfirmMessage
    { order =
        OnConfirm.Order
          { id = getId booking.id,
            state = "ACTIVE",
            items = [mkOrderItem itemCode],
            fulfillment = mkFulfillmentInfo res.fromLocation res.toLocation now, -- booking.startTime, --FIXME
            quote =
              OnConfirm.Quote
                { price =
                    OnConfirm.QuotePrice
                      { currency,
                        value = totalFareDecimal,
                        offered_value = totalFareDecimal
                      },
                  breakup =
                    mkBreakupList
                      (OnConfirm.BreakupItemPrice currency . amountToDecimalValue)
                      OnConfirm.BreakupItem
                      fareParams
                },
            payment =
              OnConfirm.Payment
                { collected_by = "BAP",
                  params =
                    OnConfirm.PaymentParams
                      { currency,
                        amount = totalFareDecimal
                      },
                  _type = OnConfirm.ON_FULFILLMENT,
                  time = OnConfirm.TimeDuration "P2D"
                }
          }
    }

mkOrderItem :: OnConfirm.ItemCode -> OnConfirm.OrderItem
mkOrderItem code =
  OnConfirm.OrderItem
    { descriptor =
        OnConfirm.Descriptor
          { code = code
          }
    }

mkFulfillmentInfo :: DBL.BookingLocation -> DBL.BookingLocation -> UTCTime -> OnConfirm.FulfillmentInfo
mkFulfillmentInfo fromLoc toLoc startTime =
  OnConfirm.FulfillmentInfo
    { state = OnConfirm.FulfillmentState "TRIP_ASSIGNED",
      start =
        OnConfirm.StartInfo
          { location =
              OnConfirm.Location
                { gps =
                    OnConfirm.Gps
                      { lat = fromLoc.lat,
                        lon = fromLoc.lon
                      },
                  address = castAddress fromLoc.address
                },
            time = OnConfirm.TimeTimestamp startTime
          },
      end =
        Just
          OnConfirm.StopInfo
            { location =
                OnConfirm.Location
                  { gps =
                      OnConfirm.Gps
                        { lat = toLoc.lat,
                          lon = toLoc.lon
                        },
                    address = castAddress toLoc.address
                  }
            }
    }
  where
    castAddress DBL.LocationAddress {..} = OnConfirm.Address {area_code = areaCode, ..}
# USSD 

##Tổ chức file:
- include:
    + ussd_define: định nghĩa record rule dùng cho việc load config
    và macro value dùng cho việc matching khi duyệt value
    + ussd_handler: chứa các hàm để load config là xử lí ussd code
- src:
    + config.xml: chứa cấu hình cho 7 dịch vụ từ 10-17 (2 dịch vụ conf và 3pty
    được gom lại do có chung mã ussd). File này được cấu thành từ các trường:
        ```xml
        <rule>
            <service key="43">
                <active cw="*43#"> </active>
                <query cw="*#43#"> </query>
                <deactive cw="#43#"> </deactive>
            </service>
        </rule>
        ```
     Để thay đổi config, đổi các giá trị của key và các trường active, query, deactive
    + ussd_server: server để xử lí bằng cách spawn các process_handle_ussd
    + ussd_client: chứa các hàm:
        - send_ussd: dùng để call hoặc cast server
        - test_all: unit test cho các hàm ở ussd_handler với tất cả các dịch vụ
        - test_ussd: unit test cho các dịch vụ ussd
        - test_10000: test hiệu năng với việc chạy 10.000 lần hàm handle_ussd.
        - test_100000: test hiệu năng với việc chạy 100.000 lần hàm handle_ussd.
        - test_1000000: test hiệu năng với việc chạy 1.000.000 lần hàm handle_ussd.
        - test_performance: test hiệu năng bằng cách sinh 1 lượng N các ussd ngẫu nhiên.

##Giải thuật:
- Input: 1 string chứa ussd code, ví dụ: `"*21*0123456789#"`
- Output: một tuple chứa trên module, function và một list các param, hoặc invalid
ví dụ: `{cfu, active, ["0123456789"]}`
- Lấy các cấu hình từ config.xml rồi chuyển sang maps để truy xuất bằng hàm *load_config*.
- Maps sẽ chứa key là giá trị key trong file config và value là một list các tuple dạng
`{active,[{cfu,"*21*TN#"}],["TN"]}` với active là function trong output,
cfu là tên module trong output, `"*21*TN#"` là mã ussd, TN là chú thích.
- Kiểm tra string đầu vào và lọc ra giá trị key bằng hàm *check_key*
- Giá trị key trên sẽ được kiểm tra trong maps
- Nếu tìm thấy trả về 1 list các tuple chứa các giá trị của dịch vụ
- Tiến hành duyệt list này để kiểm tra bằng hàm *traverse_list*
- Trong quá trình duyệt, đối với các dịch vụ có chú thích (*TN*) sẽ tiến hành dùng hàm *normalize*
để chuẩn hóa chuỗi cho việc dùng hàm *re:run* để lọc ra chú thích đó và trả về param trong output.


##Các kết quả test:
- Test hàm `ussd_client:test_100000().` kết quả được ~1 giây.
```
    Testing 100.000 cases with: 1123 milliseconds
```
- Test với client và server thông qua `ussd_client:test_performance(10000, call).`, kết quả:
```
    Time for initializing 10000 valid ussd: 63 milliseconds
    Time for initializing 10000 invalid ussd: 31 milliseconds
    Time for initializing 10000 random ussd: 31 milliseconds
    Time for processing 10000 valid ussd: 94 milliseconds
    Time for processing 10000 invalid ussd: 15 milliseconds
    Time for processing 10000 random ussd: 77 milliseconds
    Total time for processing 30000 ussd: 186 milliseconds
    Time for processing 1 ussd: 0.0062 milliseconds
```
- Test với client và server thông qua `ussd_client:test_performance(10000, cast).`, kết quả:
```
    Time for initializing 10000 valid ussd: 61 milliseconds
    Time for initializing 10000 invalid ussd: 31 milliseconds
    Time for initializing 10000 random ussd: 32 milliseconds
    Time for processing 10000 valid ussd: 109 milliseconds
    Time for processing 10000 invalid ussd: 47 milliseconds
    Time for processing 10000 random ussd: 108 milliseconds
    Total time for processing 30000 ussd: 264 milliseconds
    Time for processing 1 ussd: 0.0088 milliseconds
```
           
##Cách test:
Đầu tiên, di chuyển đến thư mục `src` 
```
cd ussd/src
erl -name thanhld@127.0.0.1 -setcookie thanhld
```
Để chạy nhiều client khác nhau, thay đổi thông số ở `-name`
``` 
cd ussd/src
erl -name thanhld1@127.0.0.1 -setcookie thanhld
```


-  Test các function ở `ussd_handler`
```
c(ussd_client),ussd_client:test_all().
```
-  Test 100.000 lần hàm `handle_ussd`
```
c(ussd_client),ussd_client:test_100000().
```

-  Test gửi nhận bản tin ussd thông qua `call` hoặc `cast` bằng `send_ussd(USSD, call|cast)`
```
c(ussd_client), c(ussd_server), ussd_server:start_link(), ussd_client:send_ussd("*63*234234", call).
hoặc
c(ussd_client), c(ussd_server), ussd_server:start_link(), ussd_client:send_ussd("*63*234234", cast).
```
- Test performance bằng việc gởi một số lượng **N** các giá trị random của ussd thông qua `call` hoặc `cast`
bằng hàm `test_performance(N, call|cast)`
```
c(ussd_client), c(ussd_server), ussd_server:start_link(), ussd_client:test_performance(10000, call).
hoặc
c(ussd_client), c(ussd_server), ussd_server:start_link(), ussd_client:test_performance(10000, cast).
```

    

   
#include<cstring>
#include<cmath>
#include<cstdio>
#include<iostream>
#include<fstream>
#include<utility>
#include<bitset>
using namespace std;
typedef long long ll;
typedef unsigned int ui;
#define test(x) cout<<(x);

class CPU
{
    public:
    ui reg[32],mem[50000],qreg[32];
    ui PC,IR,rd,rs1,rs2,imm;
    string rev;
    string revtoma;
    inline ui tar(const ui &x,int st,int en)
    {
        ui tmp=((ui)0xfffffff)>>st;
        tmp<<=st+32-en;
        tmp>>=32-en;
        return x&tmp;
    }
    inline ui Hex_DC(char buf[],int st,int en)
    {
        ui x=0;
        for(int i=st;i<=en;++i)
        {
            if(buf[i]>='A'&&buf[i]<='F')
                x=(x<<4)+buf[i]-'A'+10;
            else
                x=(x<<4)+buf[i]-'0';
        }
        return x;
    }
    CPU(string filename):PC(0),IR(0),imm(0),rs1(0),rs2(0),rd(0),rev("IUSBRJE"),revtoma("FLSJE")
    {
        fstream f;
        f.open(filename,ios::in);
        if(!f.good())
        {
            printf("Open File Error");
            return;
        }
        char buffer[100];
        while(1)
        {
            f.getline(buffer,90);
            //test(buffer);
            //test('\n');
            if(buffer[0]=='#')
                break;
            if(buffer[0]=='@')
            {
                PC=Hex_DC(buffer,1,8);
                //test(PC);
                //test('\n');
                continue;
            }
            
            int l=strlen(buffer);
            for(int i=0;i<l;i+=3)
            {    
                mem[PC++]=Hex_DC(buffer,i,i+1);
                //test(mem[PC-1]);
                //test(' ');
            }
            //test('\n');
        }
        f.close();
        PC=0;
    }
    
    enum commandtype{
        I,U,S,B,R,UJ,ERR
    };
    enum command{
        LUI=0b0110111,
        AUIPC=0b0010111,
        JAL=0b1101111,
        JALR=0b1100111+(0b000<<7),
        BEQ=0b1100011+(0b000<<7),
        BNE=0b1100011+(0b001<<7),
        BLT=0b1100011+(0b100<<7),
        BGE=0b1100011+(0b101<<7),
        BLTU=0b1100011+(0b110<<7),
        BGEU=0b1100011+(0b111<<7),
        LB=0b0000011+(0b000<<7),
        LH=0b0000011+(0b001<<7),
        LW=0b0000011+(0b010<<7),
        LBU=0b0000011+(0b100<<7),
        LHU=0b0000011+(0b101<<7),
        SB=0b0100011+(0b000<<7),
        SH=0b0100011+(0b001<<7),
        SW=0b0100011+(0b010<<7),
        ADDI=0b0010011+(0b000<<7),
        SLTI=0b0010011+(0b010<<7),
        SLTIU=0b0010011+(0b011<<7),
        XORI=0b0010011+(0b100<<7),
        ORI=0b0010011+(0b110<<7),
        ANDI=0b0010011+(0b111<<7),
        SLLI=0b0010011+(0b001<<7)+(0b0000000<<10),
        SRLI=0b0010011+(0b101<<7)+(0b0000000<<10),
        SRAI=0b0010011+(0b101<<7)+(0b0100000<<10),
        ADD=0b0110011+(0b000<<7)+(0b0000000<<10),
        SUB=0b0110011+(0b000<<7)+(0b0100000<<10),
        SLL=0b0110011+(0b001<<7)+(0b0000000<<10),
        SLT=0b0110011+(0b010<<7)+(0b0000000<<10),
        SLTU=0b0110011+(0b011<<7)+(0b0000000<<10),
        XOR=0b0110011+(0b100<<7)+(0b0000000<<10),
        SRL=0b0110011+(0b101<<7)+(0b0000000<<10),
        SRA=0b0110011+(0b101<<7)+(0b0100000<<10),
        OR=0b0110011+(0b110<<7)+(0b0000000<<10),
        AND=0b0110011+(0b111<<7)+(0b0000000<<10),
        ERROR=-1
    };
    enum tomatype{
        FP,LOAD,STORE,JUMP,NONE=-1
    };
    command cmdset[38]={LUI,AUIPC,JAL,JALR,BEQ,BNE,BLT,BGE,BLTU,BGEU,LB,LH,LW,LBU,LHU,SB,SH,SW,ADDI,SLTI,SLTIU,XORI,ORI,ANDI,SLLI,SRLI,ADD,SUB,SLL,SLT,SLTU,XOR,SRL,SRA,OR,AND};
    class input
    {
        public:
        ui x;
        command cmd;
        commandtype type;
        tomatype tomatype;
        ui A,vj,vk,qj,qk,dest;
        int time,tag;
        bool busy;
        bool solve;
        input(int t=0):x(t),type(commandtype::ERR),cmd(command::ERROR),tomatype(NONE){
            A=vj=vk=qj=qk=dest=time=busy=tag=solve=0;
        }
    };
    class queue
    {
        public:
        int rear,frt;
        input q[2000];
        int siz=2000;
        queue():rear(0),frt(0){}
        inline bool empty()
        {
            return rear==frt;
        }
        inline bool full()
        {
            return (rear+1)%siz==frt;
        }
        inline bool push(const input &x)
        {
            if(full())
            {
                cerr<<"queue full\n";
                return false;
            }
            q[++rear]=x;
            return true;
        }
        inline input front()
        {
            return q[(frt+1)%siz];
        }
        inline void pop()
        {
            if(empty()){
                cerr<<"queue full\n";
                return;
            }//MD?
            frt=(frt+1)%siz;
        }
    };
    queue ROB,IQ;
    inline commandtype get_type(command x)
    {
        switch (x) {
            case LUI:
            case AUIPC:
                return commandtype::U;
            case JAL:
                return commandtype::UJ;
            case JALR:
            case LB:
            case LH:
            case LW:
            case LBU:
            case LHU:
            case ADDI:
            case SLTI:
            case SLTIU:
            case XORI:
            case ORI:
            case ANDI:
            case SLLI:
            case SRLI:
            case SRAI:
                return commandtype::I;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                return commandtype::B;
            case SB:
            case SH:
            case SW:
                return commandtype::S;
            case ADD:
            case SUB:
            case SLL:
            case SLT:
            case SLTU:
            case XOR:
            case SRL:
            case SRA:
            case OR:
            case AND:
                return commandtype::R;
            default:
                std::cerr << "[Error]function [get_format()] wrong with a undefined CommandType." << std::endl;
                return commandtype::ERR;
        }
    }
    inline tomatype get_tomatype(command x)
    {
        switch (x)
        {
            case LB:
            case LH:
            case LW:
            case LBU:
            case LHU:
                return tomatype::LOAD;
            case SB:
            case SH:
            case SW:
                return tomatype::STORE;
            case JAL:
            case JALR:
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                return tomatype::JUMP;
            default:
                return tomatype::FP;
        }
    }
    inline ui get_opcode(ui x)
    {
        return (x&(ui)0x0000007f);
    }
    inline ui get_rs1(ui x)
    {
        return (x&(ui)0x000f8000)>>15;
    }
    inline ui get_rs2(ui x)
    {
        return (x&(ui)0x01f00000)>>20;
    }
    inline ui get_rd(ui x)
    {
        //test("getrd\n");
        //cout<<bitset<32>(x)<<endl;
        //cout<<bitset<32>((x&((ui)0x00000f80))>>7)<<endl;
        return (x&((ui)0x00000f80))>>7;
    }
    inline ui get_funct3(ui x)
    {
        return (x&(ui)0x00007000)>>12;
    }
    inline ui get_funct7(ui x)
    {
        return (x&(ui)0x0fe000000)>>25;
    }
    inline ui get_shamt(ui x)
    {
        return (x&(ui)0x01f00000)>>20;
    }
    inline ui get_immediate(ui x,commandtype type)
    {
        int ans=0,tmp=0;
        switch(type)
        {
            case R:
                break;
            case I:
                ans = (x &(ui)0xfff00000) >> 20;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfffff000;
                break;
            case S:
                ans = (x & (ui) 0xfe000000) >> 20;
                tmp = (x & (ui) 0x00000f80) >> 7;
                ans |= tmp;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfffff000;
                break;
            case B:
                ans = (x & (ui) 0x7e000000) >> 20;
                tmp = (x & (ui) 0x00000f00) >> 7;
                ans |= tmp;
                tmp = (x & (ui) 0x00000080) << 4;
                ans |= tmp;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfffff000;
                break;
            case U:
                ans = x & (ui) 0xfffff000;
                break;
            case UJ:
                ans = (x & (ui) 0x7fe00000) >> 20;
                tmp = (x & (ui) 0x000ff000);
                ans |= tmp;
                tmp = (x & (ui) 0x00100000) >> 9;
                ans |= tmp;
                if ((x & (ui) 0x80000000) == 0x80000000)ans |= 0xfff00000;
                break;
            default:
                std::cerr << "[Error]function [get_immediate()] wrong with a undefined CommandType." << std::endl;
                break;
        }
        return ans;
    }

    inline void decode(input ip)
    {   
        //cout<<bitset<32>(ip.x)<<endl;
        ui code=get_opcode(ip.x),f3=get_funct3(ip.x),f7=get_funct7(ip.x);
        for(int i=0;i<37;++i)
            if(code&cmdset[i]==code)
            {
                commandtype tmp=get_type(cmdset[i]);
                if(tmp==U||tmp==UJ)
                {   
                    ip.cmd=cmdset[i];
                    break;
                }
                if(f3&cmdset[i]!=f3)
                    continue;
                if(tmp==R&&f7&cmdset[i]!=f7)
                    continue;
                ip.cmd=cmdset[i];
                break;
            }
        ip.type=get_type(ip.cmd);
        ip.tomatype=get_tomatype(ip.cmd);
        switch (ip.tomatype)
        {
            case LOAD:
            case STORE:
                ip.time=3;
                break;
            default:
                ip.time=1;
        }
        switch (ip.type)
        {
            case R:
                rd=get_rd(ip.x);
                rs1=get_rs1(ip.x);
                rs2=get_rs2(ip.x);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                qreg[rd]=(IQ.rear+1)%IQ.siz;
                ip.dest=rd;
                break;
            case I:
                rd=get_rd(ip.x);
                rs1=get_rs1(ip.x);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                ip.dest=rd;
                qreg[rd]=(IQ.rear+1)%IQ.siz;
                if(ip.cmd==JALR)
                    ip.vk=PC;
                break;
            case S:
                rs1=get_rs1(ip.x);
                rs2=get_rs2(ip.x);
                imm=get_immediate(ip.x,ip.type);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                ip.A=imm;
                break;
            case B:
                rs1=get_rs1(ip.x);
                rs2=get_rs2(ip.x);
                imm=get_immediate(ip.x,ip.type);
                if(qreg[rs1])
                    ip.qj=qreg[rs1];
                if(qreg[rs2])
                    ip.qk=qreg[rs2];
                ip.A=imm&0;//SP
                ip.dest=PC;
                break;
            case U:
            case UJ:
                rd=get_rd(ip.x);
                imm=get_immediate(ip.x,ip.type);
                ip.vk=PC;
                ip.A=imm;
                ip.dest=rd;
                qreg[rd]=(IQ.rear+1)%IQ.siz;
                break;
            default:
                cerr<<"Error at [decoding]: dicover unexpected commandtype"<<endl; 
                break;
        }
        ROB.push(ip);
        ip.tag=ROB.rear;
        IQ.push(ip);
        ROB.q[ROB.rear].tag=IQ.rear;
        printf("[DECODING...]PC:%u opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%u\n",PC,ip.cmd,rev[ip.type],revtoma[ip.tomatype],IQ.q[ip.tag].vj,IQ.q[ip.tag].vk,ip.dest,ip.A);
        fstream f("output.data",ios::out|ios::app);
        f<<code<<" ";
        f.close();

    }
    inline void sign_extend(ui &x,int pos){
        x|=(((ui)0xfffffff)>>pos)<<pos;
    }
    inline void IF()
    {
        ui x=0;
        for(int i=3;i>=0;--i)
            x=(x<<8)|mem[PC+i];
        decode(input(x));
    }
    inline ui EXE(input ip)//MD?
    {
        printf("[EXECUTING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%u\n",ip.cmd,rev[ip.type],revtoma[ip.tomatype],IQ.q[ip.tag].vj,IQ.q[ip.tag].vk,ip.dest,ip.A);
        ui ans;
        switch (ip.cmd)
        {
            case LUI:
                break;
            case AUIPC:
                ip.A=ip.vj+ip.A;
                break;
            case JAL:
                break;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                ip.A+=ip.dest;
                break;
            case JALR:
                ip.A=ip.A+ip.vj;
                break;
            case LB:
                ip.A=ip.vj+(int)ip.A;
                break;
            case LH:
                ip.A=ip.vj+(int)ip.A;
                break;    
            case LW:
                ip.A=ip.vj+(int)ip.A;
                break; 
            case LBU:
                ip.A=ip.vj+(int)ip.A;
                break;
            case LHU:
                ip.A=ip.vj+(int)ip.A;
                break;
            case SB:
                ip.A=ip.vj+(int)ip.A;
                break;
            case SH:
                ip.A=ip.vj+(int)ip.A;
                break;
            case SW:
                ip.A=ip.vj+(int)ip.A;
                break;                                                                 
            case ADDI:
                ip.A=ip.vj+(int)ip.A;
                break;
            case SLTI:
                ip.A=(int)ip.vj<(int)ip.A;
                break;
            case SLTIU:
                ip.A=ip.vj<ip.A;
                break;
            case XORI:
                ip.A=ip.vj^ip.A;
                break;
            case ORI:
                ip.A=ip.vj|ip.A;
                break;
            case ANDI:
                ip.A=ip.vj&ip.A;
                break;
            case SLLI://SP?
                ip.A=ip.vj<<tar(ip.A,0,5);
                break;
            case SRLI://SP?
                ip.A=ip.vj<<tar(ip.A,0,5);
                break;
            case SRAI://?????
                ip.A=ip.vj>>tar(ip.A,0,5);
                sign_extend(ip.A,31-tar(ip.A,0,5));//CHECK?
                break;
            case ADD:
                ip.A=ip.vj+ip.vk;
                break;
            case SUB:
                ip.A=ip.vj-ip.vk;
                break;
            case SLL://SP?
                ip.A=ip.vj<<tar(ip.vk,0,5);
                break;
            case SLT:
                ip.A=(int)ip.vj<(int)ip.vk;
                break;
            case SLTU:
                ip.A=ip.vj<ip.vk;
                break;
            case XOR:
                ip.A=ip.vj^ip.vk;
                break;
            case SRL:
                ip.A=ip.vj>>tar(ip.vk,0,5);
                break;
            case SRA:
                ip.A=ip.vj>>tar(ip.A,0,5);
                sign_extend(ip.A,31-tar(ip.A,0,5));//CHECK?
                break;
            case OR:
                ip.A=ip.vj|ip.vk;
                break;
            case AND:
                ip.A=ip.vj&ip.vk;
                break;
            default:
                cerr<<"unexpected command at [EXE]"<<endl;
        }
        return ans;
    }
    inline ui WB(input ip)//MD?
    {
        ui ans;
        ROB.q[ip.tag].busy=true;
        printf("[WBING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%u\n",ip.cmd,rev[ip.type],revtoma[ip.tomatype],IQ.q[ip.tag].vj,IQ.q[ip.tag].vk,ip.dest,ip.A);
        switch (ip.cmd)
        {
            case BEQ:
                if(ip.vj!=ip.vk)
                    break;
            case BNE:
                if(ip.vj==ip.vk)
                    break;
            case BLT:
                if((int)ip.vj>=(int)ip.vk)
                    break;
            case BGE:
                if((int)ip.vj<(int)ip.vk)
                    break;
            case BLTU:
                if(ip.vj>=ip.vk)
                    break;
            case BGEU:
                if(ip.vj<ip.vk)
                    break;
            default:
                ROB.q[ip.tag].A=ip.A;
        }
        return ans;       
    }
    inline ui COMMIT(input ip)//MD?
    {
        ui ans;
        printf("[COMMITING...]opcode:%u type:%c tomatype:%c rs1:%u rs2:%u rd:%u imm:%u\n",ip.cmd,rev[ip.type],revtoma[ip.tomatype],IQ.q[ip.tag].vj,IQ.q[ip.tag].vk,ip.dest,ip.A);
        switch (ip.cmd)
        {
            case LUI:
                reg[ip.dest]=ip.A;
                break;
            case AUIPC:
                reg[ip.dest]=ip.A;
                break;
            case JAL:
            case JALR:
                reg[ip.dest]=PC+4;
                PC=ip.A;//MD
                break;
            case BEQ:
            case BNE:
            case BLT:
            case BGE:
            case BLTU:
            case BGEU:
                PC=ip.A;
                break;
            case LB:
                reg[ip.dest]=mem[ip.A];
                sign_extend(reg[ip.dest],7);
                break;
            case LH:
                reg[ip.dest]=mem[ip.A+1];
                reg[ip.dest]=(reg[ip.dest]<<8)|mem[ip.A];
                sign_extend(reg[ip.dest],15);
                break;    
            case LW:
                reg[ip.dest]=0;
                for(int i=3;i>=0;--i)
                    reg[ip.dest]=(reg[ip.dest]<<8)|mem[ip.A+i];
                break; 
            case LBU:
                reg[ip.dest]=mem[ip.A];
                break;
            case LHU:
                reg[ip.dest]=mem[ip.A+1];
                reg[ip.dest]=(reg[ip.dest]<<8)|mem[ip.A];
                break;
            case SB:
                mem[ip.A]=tar(ip.vk,0,7);
                break;
            case SH:
                mem[ip.A+1]=tar(ip.vk,0,7);
                mem[ip.A]=tar(ip.vk,8,15);
                break;
            case SW:
                for(int i=3;i>=0;--i)
                    mem[ip.A+i]=tar(ip.vk,8*(3-i),8*(4-i)-1);
                break;                                                                 
            case ADDI:
            case SLTI:
            case SLTIU:
            case XORI:
            case ORI:
            case ANDI:
            case SLLI:
            case SRLI:
            case SRAI:
            case ADD:
            case SUB:
            case SLL:
            case SLT:
            case SLTU:
            case XOR:
            case SRL:
            case SRA:
            case OR:
            case AND:
                reg[ip.dest]=ip.A;
                break;
            default:
                cerr<<"unexpected command at [COMMIT]"<<endl;
        }
        return ans;
    }
    inline void broadcast(int i)
    {
        if(IQ.q[i].tomatype!=STORE)
        {
            for(int j=(IQ.frt+1)%IQ.siz;j!=IQ.rear;j=(j+1)%IQ.siz)
            {
                if(IQ.q[j].qk==i)
                {   
                    IQ.q[j].vk=IQ.q[i].A; 
                    IQ.q[j].qk=0;
                }
                if(IQ.q[j].qj==i)
                {   
                    IQ.q[j].vj=IQ.q[i].A; 
                    IQ.q[j].qj=0;
                }
            }
            ROB.q[IQ.q[i].tag].busy=true;
            ROB.q[IQ.q[i].tag].A=IQ.q[i].A;
        }
        else
        {
            for(int j=(IQ.frt+1)%IQ.siz;j!=IQ.rear;j=(j+1)%IQ.siz)
            {
                if(IQ.q[j].qj==ROB.front().tag)
                    IQ.q[j].qj=0;
            }
        }
        for(int j=0;j<32;++j)
            if(qreg[j]==i)
                qreg[j]=0;
    }
    inline ui upd()
    {
        for(int i=(IQ.frt+1)%IQ.siz;i!=IQ.rear;i=(i+1)%IQ.siz)
        {
            printf("[QUEUE UPDATING...]opcode:%u type:%c tomatype:%c qj:%u qk:%u imm:%u\n",IQ.q[i].cmd,rev[IQ.q[i].type],revtoma[IQ.q[i].tomatype],IQ.q[i].qj,IQ.q[i].qk,IQ.q[i].dest,IQ.q[i].A);
            if(IQ.q[i].time==-1)
                continue;
            if(!IQ.q[i].time)
            {
                WB(IQ.q[i]);
                if(IQ.q[i].tomatype!=STORE)
                    broadcast(i);
                --IQ.q[i].time;//to make it -1
                continue;
            }
            if((!IQ.q[i].qj)&&(!IQ.q[i].qk)&&(!IQ.q[i].solve))
            {
                EXE(IQ.q[i]);
                if((IQ.q[i].tomatype==LOAD||IQ.q[i].tomatype==STORE))
                { 
                    IQ.q[i].solve=true;
                    ROB.q[IQ.q[i].tag].A=IQ.q[i].A;
                    for(int j=(ROB.frt+1)%ROB.siz;j!=IQ.q[i].tag;j=(j+1)%ROB.siz)
                    {
                        if(IQ.q[j].tomatype==STORE&&ROB.q[j].A==IQ.q[i].A)
                        {
                            IQ.q[i].qj=ROB.q[j].tag;
                            break;
                        }
                    }
                    continue;
                }
                //EXE(IQ.q[i]);
                IQ.q[i].busy=true; 
            }
            if(IQ.q[i].busy)
                --IQ.q[i].time;
        }
        if((!ROB.empty())&&ROB.front().busy)
        {
            if(ROB.front().x==((ui)0x0ff00513))
                return reg[10]&((ui)255);
            if(ROB.front().tomatype==JUMP&&ROB.front().A)
            {    
                ROB.rear=(ROB.frt+1)%ROB.siz;    
                IQ.rear=(IQ.frt+1)%IQ.siz;//Q.clear()
            }
            if(ROB.front().tomatype==STORE)
                broadcast(ROB.front().tag);
            COMMIT(ROB.front());
            ROB.pop();
            IQ.pop();//MD?
        }
        reg[0]=0;//SP
        return 0;
    }
    inline void print()
    {
        for(int i=0;i<20;++i)
            printf("%d ",mem[i]);
        return;
    }
};

int main()
{
    fstream f("output.data",ios::out|ios::trunc);
    f.close();
    CPU T("sample.data");
    for(int i=1;i<=10;++i)
    {
        printf("[Cycle %d]\n",i);
        T.IF();
        //cout<<T.PC<<endl;
        int tmp=T.upd();
        if(tmp)
        {    
            cout<<tmp<<endl;
            break;
        }
        T.PC+=4;
    }
    return 0;
}